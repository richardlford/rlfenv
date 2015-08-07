package [[full-module-name]];

use 5.6.1;
use strict;
use warnings;
use Carp qw(carp cluck confess croak);

BEGIN {
    use Exporter   ();
    our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

    $VERSION     = 0.01;
    @ISA         = qw(Exporter);

    %EXPORT_TAGS = ( 'all' => [ qw(

    ) ] );

    @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

    @EXPORT = qw(

    );
}

=head1 NAME

[[full-module-name]] - zapp

=head1 SYNOPSIS

  use [[full-module-name]];
  zapp

=head1 ABSTRACT

  zapp

=head1 DESCRIPTION

zapp

=head2 EXPORT

None by default.


=head1 AUTHOR

Richard L. Ford, E<lt>richard.l.ford@intel.comE<gt>

=head1 EXPORTED PACKAGE GLOBALS

=over 4

=cut

# exported package globals go here

=back

=head1 NON-EXPORTED PACKAGE GLOBALS

=over 4

=cut

# non-exported package globals go here

=back

=cut

# initialize package globals, first exported ones

# then the others (which are still accessible as $Some::Module::stuff)

# all file-scoped lexicals must be created before
# the functions below that use them.

=head1 METHODS

=over 4

=cut

#methods-zapp

=back

=cut

END { }       # module clean-up code here (global destructor)

=head1 SEE ALSO

=cut

1;
__END__
