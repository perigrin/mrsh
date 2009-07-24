package App::MrShell;
use Moose;

use POSIX;
use Config::Tiny;
use POE qw( Wheel::Run );
use Term::ANSIColor qw(:constants);
use Text::Balanced;

our $VERSION = '2.0205';

has hosts => (
    isa     => 'ArrayRef',
    is      => 'ro',
    builder => '_build_hosts',
);

sub _build_hosts { [] }

has cmd => (
    isa     => 'ArrayRef',
    is      => 'ro',
    builder => '_build_cmd',
);

sub _build_cmd { [] }

has _shell_cmd => (
    isa     => 'ArrayRef',
    is      => 'ro',
    builder => '_build__shell_cmd',
);

sub _build__shell_cmd {
    [
        ssh => '-o', 'BatchMode yes', '-o', 'StrictHostKeyChecking no', '-o',
        'ConnectTimeout 20', '[%u]-l', '[]%u', '%h'
    ];
}

# _process_space_delimited {{{
sub _process_space_delimited {
    my $this = shift;
    my $that = shift;

    my @output;
    while ($that) {
        if ( $that =~ m/^\s*['"]/ ) {
            my ( $tok, $rem )
                = Text::Balanced::extract_delimited( $that, qr(["']) );

            ( $tok =~ s/^(['"])// and $tok =~ s/$1$// )
                or die "internal error processing space delimited";

            push @output, $tok;
            $that = $rem;

        }
        else {
            my ( $tok, $rem ) = split m/\s+/, $that, 2;

            push @output, $tok;
            $that = $rem;
        }
    }

    return @output;
}

# }}}
# _process_hosts {{{
sub _process_hosts {
    my $this = shift;

    my @h = do {
        my @tmp = map {
            my $k = $_;
            $k =~ s/^\@//
                ? @{
                $this->{groups}{$k}
                    or die "couldn't find group: \@$k\n"
                }
                : $_
        } @_;
        my %h;
        @h{@tmp} = ();
        for ( keys %h ) {
            if ( my ($k) = m/^\-(.+)/ ) {
                delete $h{$_};
                delete $h{$k};
            }
        }
        sort keys %h;
    };

    my $o = my $l = $this->{_host_width} || 0;
    for ( map { length $this->_host_route_to_nick($_) } @h ) {
        $l = $_ if $_ > $l;
    }

    $this->{_host_width} = $l if $l != $o;

    return @h;
}

# }}}
# _host_route_to_nick {{{
sub _host_route_to_nick {
    my $this = shift;

    return join "", shift =~ m/(?:!|[^!]+$)/g;
}

# }}}

# set_shell_command_option {{{
sub set_shell_command_option {
    my $this = shift;
    my $arg  = shift;

    if ( ref($arg) eq "ARRAY" ) {
        $this->{_shell_cmd} = [@$arg];    # make a real copy

    }
    else {
        $this->{_shell_cmd}
            = [ $this->_process_space_delimited( $arg || "" ) ];
    }

    return $this;
}

# }}}
# set_group_option {{{
sub set_group_option {
    my $this = shift;
    my $groups = ( $this->{groups} ||= {} );

    my ( $name, $value );
    while ( ( $name, $value ) = splice @_, 0, 2 and $name and $value ) {
        if ( ref($value) eq "ARRAY" ) {
            $groups->{$name} = [@$value];    # make a real copy

        }
        else {
            $groups->{$name} = [ $this->_process_space_delimited($value) ];
        }
    }

    my @groups        = keys %{ $this->{groups} };
    my $replace_limit = 30;
REPLACE_GROPUS: {
        my $replaced = 0;

        for my $group (@groups) {
            my $hosts = $groups->{$group};

            my $r = 0;
            for (@$hosts) {
                if (m/^@(.+)/) {
                    if ( my $g = $groups->{$1} ) {
                        $_ = $g;

                        $r++;
                    }
                }
            }

            if ($r) {
                my %h;
                @h{ map { ref $_ ? @$_ : $_ } @$hosts } = ();
                $groups->{$group} = [ keys %h ];
                $replaced++;
            }
        }

        $replace_limit--;
        last if $replace_limit < 1;
        redo if $replaced;
    }

    return $this;
}

# }}}
# set_logfile_option {{{
sub set_logfile_option {
    my $this  = shift;
    my $file  = shift;
    my $trunc = shift;

    unless ( our $already_compiled++ ) {
        my $load_ansi_filter_package = q {
            package App::MrShell::ANSIFilter;
            use Symbol;
            use Tie::Handle;
            use base 'Tie::StdHandle';

            my %orig;

            sub PRINT {
                my $this = shift;
                my @them = @_;
                s/\e\[[\d;]+m//g for @them;
                print {$orig{$this}} @them;
            }

            sub filtered_handle {
                my $pfft = gensym();
                my $it = tie *{$pfft}, __PACKAGE__ or die $!;
                $orig{$it} = shift;
                $pfft;
            }

        1};

        eval $load_ansi_filter_package
            or die $@;    ## no critic -- sometimes this kind of eval is ok
                          # (This probably isn't one of them.)
    }

    open my $log, ( $trunc ? ">" : ">>" ), $file
        or confess "couldn't open $file for write: $!"
        ;                 ## no critic -- I mean to pass this around, shut up

    $this->{_log_fh} = App::MrShell::ANSIFilter::filtered_handle($log);

    return $this;
}

# }}}
# set_debug_option {{{
sub set_debug_option {
    my $this = shift;
    my $val  = shift;

    # -d 0 and -d 1 are the same
    # -d 2 is a level up, -d 4 is even more
    # $val==undef clears the setting

    if ( not defined $val ) {
        delete $this->{debug};
        return $this;
    }

    $this->{debug} = $val ? $val : 1;

    return $this;
}

# }}}
# set_no_command_escapes_option {{{
sub set_no_command_escapes_option {
    my $this = shift;

    $this->{no_command_escapes} = shift || 0;

    return $this;
}

# }}}

# groups {{{
sub groups {
    my $this = shift;

    return unless $this->{groups};
    return wantarray ? %{ $this->{groups} } : $this->{groups};
}

# }}}

# set_usage_error($&) {{{
sub set_usage_error($&) {    ## no critic -- prototypes are bad how again?
    my $this = shift;
    my $func = shift;
    my $pack = caller;
    my $name = $pack . "::$func";
    my @args = @_;

    $this->{_usage_error} = sub {
        no strict 'refs'
            ;    ## no critic -- how would you call this by name without this?
        $name->(@args);
    };

    return $this;
}

# }}}
# read_config {{{
sub read_config {
    my ( $this, $that ) = @_;

    $this->{_conf} = Config::Tiny->read($that) if -f $that;

    for my $group ( keys %{ $this->{_conf}{groups} } ) {
        $this->set_group_option( $group => $this->{_conf}{groups}{$group} );
    }

    if ( my $c = $this->{_conf}{options}{'shell-command'} ) {
        $this->set_shell_command_option( 1, $c );
    }

    if ( my $c = $this->{_conf}{options}{'logfile'} ) {
        my $t = $this->{_conf}{options}{'truncate-logfile'};
        my $v = ( $t ? 1 : 0 );
        $v = 0 if $t =~ m/(?:no|false)/i;

        $this->set_logfile_option( $c, $v );
    }

    if ( my $c = $this->{_conf}{options}{'no-command-escapes'} ) {
        my $v = ( $c ? 1 : 0 );
        $v = 0 if $c =~ m/(?:no|false)/i;

        $this->set_no_command_escapes_option($v);
    }

    return $this;
}

# }}}
# set_hosts {{{
sub set_hosts {
    my $this = shift;

    $this->{hosts} = [ $this->_process_hosts(@_) ];

    return $this;
}

# }}}
# queue_command {{{
sub queue_command {
    my $this  = shift;
    my @hosts = @{ $this->{hosts} };

    unless (@hosts) {
        if ( my $h = $this->{_conf}{options}{'default-hosts'} ) {
            @hosts = $this->_process_hosts(
                $this->_process_space_delimited($h) );

        }
        else {
            if ( my $e = $this->{_usage_error} ) {
                warn "Error: no hosts specified\n";
                $e->();

            }
            else {
                confess "set_hosts before issuing queue_command";
            }
        }
    }

    for my $h (@hosts) {
        push @{ $this->{_cmd_queue}{$h} }, [@_];    # make a real copy
    }

    return $this;
}

# }}}
# run_queue {{{
sub run_queue {
    my $this = shift;

    $this->{_session} = POE::Session->create(
        inline_states => {
            _start       => sub { $this->poe_start(@_) },
            child_stdout => sub { $this->line( 1, @_ ) },
            child_stderr => sub { $this->line( 2, @_ ) },
            child_signal => sub { $this->sigchld(@_) },
            stall_close  => sub { $this->_close(@_) },
            ErrorEvent   => sub { $this->error_event },
        }
    );

    POE::Kernel->run();

    return $this;
}

# }}}

# std_msg {{{
sub std_msg {
    my $this  = shift;
    my $host  = shift;
    my $cmdno = shift;
    my $fh    = shift;
    my $msg   = shift;

    my $host_msg = $host ? $this->_host_route_to_nick($host) . ": " : "";
    my $time_str = strftime( '%H:%M:%S', localtime );

    print $time_str,
        sprintf( ' %-*s', $this->{_host_width} + 2, $host_msg ),
        ( $fh == 2 ? ( '[', BOLD, YELLOW, 'stderr', RESET, '] ' ) : () ),
        $msg, RESET, "\n";

    if ( $this->{_log_fh} ) {
        $time_str = strftime( '%Y-%m-%d %H:%M:%S', localtime );

        # No point in printing colors, stripped anyway.  Formatting columns is
        # equally silly -- in append mode anyway.
        $host_msg = $host ? "$host: " : "";
        print { $this->{_log_fh} } "$time_str $host_msg",
            ( $fh == 2 ? "[stderr] " : "" ), $msg, "\n";
    }

    return $this;
}

# }}}

# line {{{
sub line {
    my $this = shift;
    my $fh   = shift;
    my ( $line, $wid ) = @_[ ARG0, ARG1 ];
    my ( $kid, $host, $cmdno, $lineno ) = @{ $this->{_wid}{$wid} };

    $$lineno++;
    $this->std_msg( $host, $cmdno, $fh, $line );

    return;
}

# }}}

# sigchld {{{
sub _sigchld_exit_error {
    my $this = shift;
    my ( $pid, $exit ) = @_[ ARG1, ARG2 ];
    $exit >>= 8;

    $this->std_msg(
        "?", -1, 0,
        BOLD 
            . RED
            . "-- sigchld received for untracked pid($pid, $exit), probably a bug in Mr. Shell --"
    );

    return;
}

sub sigchld {
    my $this = shift;    # ARG0 is the signal name string
    my ( $kid, $host, $cmdno, @c )
        = @{ $this->{_pid}{ $_[ARG1] }
            || return $this->_sigchld_exit_error(@_) };

    # NOTE: this usually isn't an error, sometimes the sigchild will arrive
    # before the handles are "closed" in the traditional sense.  We get error
    # eveents for errors.
    #### # $this->std_msg($host, $cmdno, 0, RED.'-- error: unexpected child exit --');

    # NOTE: though, the exit value may indicate an actual error.
    if ( ( my $exit = $_[ARG2] ) != 0 ) {

        # XXX: I'd like to do more here but I'm waiting to see what Paul
        # Fenwick has to say about it.
        $exit >>= 8;

        my $reset = RESET;
        my $black = BOLD . BLACK;
        my $red   = RESET . RED;

        $this->std_msg(
            $host, $cmdno, 0,
            "$black-- shell exited with nonzero status: $red$exit$black --"
        );
    }

    $_[KERNEL]->yield( stall_close => $kid->ID, 0 );

    return;
}

# }}}
# _close {{{
sub _close {
    my $this = shift;
    my ( $wid, $count ) = @_[ ARG0, ARG1 ];

    return
        unless $this->{_wid}
        {$wid};    # sometimes we'll get a sigchild *and* a close event

    # NOTE: I was getting erratic results with some fast running commands and
    # guessed that I was sometimes getting the close event before the stdout
    # event. Waiting through the kernel loop once is probably enough, but I
    # used 3 because it does't hurt either.

    if ( $count > 3 ) {
        my ( $kid, $host, $cmdno, $lineno, @c )
            = @{ delete $this->{_wid}{$wid} };

        $this->std_msg( $host, $cmdno++, 0, BOLD . BLACK . '-- eof --' )
            if $$lineno == 0;
        if (@c) {
            $this->start_queue_on_host( $_[KERNEL] => $host, $cmdno, @c );
            $this->std_msg(
                $host, $cmdno, 0,
                BOLD . BLACK . "-- starting: @{$c[0]} --"
            );
        }

        delete $this->{_pid}{ $kid->PID };

    }
    else {
        $_[KERNEL]->yield( stall_close => $wid, $count + 1 );
    }

    return;
}

# }}}
# error_event {{{
sub error_event {
    my $this = shift;
    my ( $operation, $errnum, $errstr, $wid ) = @_[ ARG0 .. ARG3 ];
    my ( $kid, $host, $cmdno, @c )
        = @{ delete $this->{_wid}{$wid} || return };
    delete $this->{_pid}{ $kid->PID };

    $errstr = "remote end closed" if $operation eq "read" and not $errnum;
    $this->std_msg(
        $host, $cmdno, 0,
        RED . "-- $operation error $errnum: $errstr --"
    );

    return;
}

# }}}

# set_subst_vars {{{
sub set_subst_vars {
    my $this = shift;

    while ( my ( $k, $v ) = splice @_, 0, 2 ) {
        $this->{_subst}{$k} = $v unless exists $this->{_subst}{$k};
    }

    return $this;
}

# }}}
# subst_cmd_vars {{{
sub subst_cmd_vars {
    my $this = shift;
    my %h    = %{ delete( $this->{_subst} ) || {} };
    my $host = $h{'%h'};

    my @c = @_;    # copy this so it doesn't get altered upstream
                   # (I'd swear I shoulnd't need to do this at all, but it's
                   #  proovably true that I do.)

    if ( $host =~ m/\b(?!<\\)!/ ) {
        my @hosts = split '!', $host;

        my @indexes_of_replacements;
        for ( my $i = 0; $i < @c; $i++ ) {
            if ( $c[$i] eq '%h' ) {
                splice @c, $i, 1, $hosts[0];

                push @indexes_of_replacements, $i;

                for my $h ( reverse @hosts[ 1 .. $#hosts ] ) {
                    splice @c, $i + 1, 0, @c[ 0 .. $i - 1 ] => $h;
                    push @indexes_of_replacements,
                        $i + 1 + $indexes_of_replacements[-1];

                    unless ( $this->{no_command_escapes} ) {
                        for my $arg ( @c[ $i + 1 .. $#c ] ) {

             # NOTE: This escaping is going to be an utter pain to maintain...

                            $arg =~ s/([`\$])/\\$1/g;

                            if ( $arg =~ m/[\s()]/ ) {
                                $arg =~ s/([\\"])/\\$1/g;
                                $arg = "\"$arg\"";
                            }
                        }
                    }
                }
            }
        }

        my $beg = 0;
        for my $i (@indexes_of_replacements) {
            if ( $c[$i] =~ s/^([\w.\-_]+)@// ) {
                my $u = $1;
                for ( @c[ $beg .. $i - 1 ] ) {
                    s/^(\[\%u\]|\[\](?=\%u))//;
                    $_ = $u if $_ eq '%u';
                }

            }
            else {

                # NOTE: there's really no need to go through and remove [%u]
                # conditional options, they'll automatically get nuked below
                $c[$i] =~ s/\\@/@/g;
            }
            $beg = $i + 1;
        }

        delete $h{'%h'};

    }
    else {
        $h{'%h'} =~ s/\\!/!/g;
    }

    if ( $h{'%h'} ) {
        $h{'%u'} = $1 if $h{'%h'} =~ s/^([\w.\-_]+)@//;
        $h{'%h'} =~ s/\\@/@/g;
    }

    @c = map { exists $h{$_} ? $h{$_} : $_ }
        map {
        m/^\[([^\[\]]+)\]/
            ? ( $h{$1} ? do { s/^\[\Q$1\E\]//; $_ } : () )
            : ($_)
        }    ## no critic: why on earth not?
        map {
        s/\[\]\%(\w+)/[\%$1]\%$1/;
        $_
        }    ## no critic: why on earth not?
        @c;

    if ( $this->{debug} ) {
        local $" = ")(";
        $this->std_msg(
            $host, $h{'%n'}, 0,
            BOLD . BLACK . "DEBUG: exec(@c)"
        );
    }

    return @c;
}

# }}}
# start_queue_on_host {{{
sub start_queue_on_host {
    my ( $this, $kernel => $host, $cmdno, $cmd, @next ) = @_;

    # NOTE: used (and deleted) by subst_cmd_vars
    $this->set_subst_vars(
        '%h' => $host,
        '%n' => $cmdno,
    );

    my $kid = POE::Wheel::Run->new(
        Program => [
            my @debug_rq = (
                $this->subst_cmd_vars( @{ $this->{_shell_cmd} } => @$cmd )
            )
        ],
        StdoutEvent => "child_stdout",
        StderrEvent => "child_stderr",
        CloseEvent  => "child_close",
    );

    $kernel->sig_child( $kid->PID, "child_signal" );

    my $lineno = 0;
    my $info = [ $kid, $host, $cmdno, \$lineno, @next ];
    $this->{_wid}{ $kid->ID } = $this->{_pid}{ $kid->PID } = $info;

    return;
}

# }}}

# poe_start {{{
sub poe_start {
    my $this = shift;

    my %starting;
    my @hosts = keys %{ $this->{_cmd_queue} };
    for my $host (@hosts) {
        my @c = @{ $this->{_cmd_queue}{$host} };

        $this->start_queue_on_host( $_[KERNEL] => $host, 1, @c );
        push @{ $starting{"@{$c[0]}"} }, $host;
    }

    for my $message ( keys %starting ) {
        my @hosts = @{ $starting{$message} };

        if ( @hosts == 1 ) {
            $this->std_msg(
                $this->_host_route_to_nick( $hosts[0] ), 1, 0,
                BOLD . BLACK . "-- starting: $message --"
            );

        }
        else {
            $this->std_msg(
                "", 1, 0,
                BOLD . BLACK . "-- starting: $message on @hosts --"
            );
        }
    }

    delete $this->{_cmd_queue};

    return;
}

# }}}

1;
__END__

=head1 NAME

App::MrShell - do everything the mrsh commandline tool can do and more

=head1 SYNOPSIS

    my $mrsh = App::MrShell
        -> new
        -> set_hosts('host1', 'host2', 'host3')
        -> queue_command('uptime')
        -> queue_command('mii-tool', 'eth0')
        -> queue_command('dmesg | head')
        -> run_queue;

=head1 DESCRIPTION

This package can do all the things L<mrsh> can do and possibly more.  The
methods mostly support chaining to make script writing easier and more
attractive.  The API hides the L<POE> nature of the project, but the author is
not opposed to exposing it if anybody wants such a thing.

=head1 OPTIONS METHODS

=over

=item B<set_shell_command_option>

The first argument is normally a space separated list of command-and-arguments
(with some basic quoting support).   If called with no arguments, a null-command
will be set indicating that there isn't a shell command to prepend hostnames
with -- the author can't imagine why this would be useful, but allows that it
could be someday for someone.

If the first argument is an arrayref, all following arguments will be ignored.
The arrayref will be copied as the shell command.  C<[]> can be used as the
arrayref to set a null-command.

=item B<set_group_option>

Setup groups of hosts.  Pairs are L<spliced|perlfunc/splice> off the stack with
first element as the name of a group, the second element as either a space
delimited set of hostnames or an arrayref of hostnames.  Example:

    $mrsh->set_group_option( group1 => [qw(host1 host2 host3)],
        group2=>['localhost'] );

=item B<set_logfile_option>

When provided, write a logfile of all lines received, where and when they were
executed.

=item B<set_debug_option>

Turn on various debugging messages.  The optional argument specifies the debug
level.  No argument, 0, and 1 are all equivalent.  While levels greater than one
indicate an increased amount of debugging noise.

=item B<set_no_command_escapes_option>

When expanding hosts in host-routing mode, slashes and spaces are escaped so
they function correctly when subshelled.  This disables that functionality.

=item B<read_config>

The options above can be specified from a L<config file|mrsh/CONFIG FILE>, which
is documented in the command line tool.  The config file is read using
L<Config::Tiny>.

=item B<set_usage_error>

Pass in a function name to call on error.  Useful for showing
L<pod2usage|Pod::Usage/pod2usage>() information.  Example:

    my $mrsh = App::MrShell->new->set_usage_error("pod2usage");

=back

=head1 COMMAND METHODS

=over

=item B<set_hosts>

Set the hosts, groups, and routed strings for the next L</queue_command>.  Hosts
have some special magic concerning groups.  Hosts that being with an C<@>
character are considered groups are are expanded that way (see
L<config file|mrsh/[groups]>).

Also see the section on L<--host|mrsh/--host -H> in the L<mrsh> documentation.

=item B<queue_command>

Push a command into the stack for the given hosts (specified by L</set_hosts> or
by the default hosts in the L<config file|mrsh/CONFIG FILE>).

Commands are pushed as an array, although, given the nature of
L<http://openssh.com/> it's probably ok to pass in the command as a single
string; or even to pass in multiple commands, C<;> separated and let the shell
sort it out.

=item B<set_subst_vars>

Set any built in or extra subst vars.  These will only ever be set once per
L</subst_cmd_vars>() call, so setting built-in keys will override the built-in
values, with the exception of C<%u> at least and C<%u> during host-routing mode
-- since those are special subst vars, their substitutions will be difficult to
override.

=item B<subst_cmd_vars>

Substitute command vars, including the special magic of host routing and the
escaping associated with it (e.g. C<%h> for the hostname).   These are better
documented in the L<COMMAND ESCAPES|mrsh/COMMAND ESCAPES> section.

Others (that is non-internal substs) can be added with L</set_subst_vars>(),
although C<%h> and C<%u> are magic and setting them manually probably won't
work.

As arguments, the function takes the command, upon which substitution should
occur, as an array.

=item B<run_queue>

Run whatever commands are queued.  This starts a L<POE::Session> and issues a
L<POE::Kernel>->L<run|POE::Kernel/run>().

=back

=head1 VARS

=over

=item B<@DEFAULT_SHELL_COMMAND>

This can be specified, perhaps better, via L</set_shell_command_option>() or via
the L<config file|mrsh/CONFIG FILE>.

=back

=head1 REPORTING BUGS / REQUEST FEATURES

You can report bugs either via L<http://rt.cpan.org/> or via the issue tracking
system on github (L<http://github.com/jettero/mrsh/issues>).  I'm likely to
notice either fairly quickly.

For feature requests, just go ahead and email me.  I've never minded a
discussion of this nature yet.

=head1 AUTHOR

Paul Miller C<< <jettero@cpan.org> >>

=head1 COPYRIGHT

Copyright 2009 Paul Miller -- released under the GPL

=head1 SEE ALSO

ssh(1), perl(1), L<mrsh>, L<POE>, L<POE::Wheel::Run>, L<Term::ANSIColor>
