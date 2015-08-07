# This is template new.pm. Others available are:
#
# our.pm - Variable declared with our.
# my.pm  - Variable declared with my.
# sub.pm - subroutine


package [[full-module-name]];

use 5.006;
use strict;
use warnings;
use Carp qw(carp cluck confess croak);

# The following Exporter stuff would be used for a non-object-oriented module.
# An object oriented module would just export a 'new' class method that would
# construct an object of the class. Then the methods of the class would be
# invoked through the method.
BEGIN {
    use Exporter   ();
    our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);
    # set the version for version checking
    #$VERSION     = 1.00;
    # if using RCS/CVS, this may be preferred
    $VERSION = do { my @r = (q$Revision: 1.1 $ =~ /\d+/g); sprintf "%d."."%02d" x $#r, @r }; # must be all one line, for MakeMaker
    @ISA         = qw(Exporter);


    # This allows declaration       use [[full-module-name]] ':all';
    # If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
    # will save memory.
    %EXPORT_TAGS = ( 'all' => [ qw(

    ) ] );

    @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

    # Items to export into callers namespace by default. Note: do not export
    # names by default without a very good reason. Use EXPORT_OK instead.
    # Do not simply export all your public functions/methods/constants.
    @EXPORT = qw(
        $Var1 %Hashit &func3
    );
}

# Below is stub documentation for your module. You better edit it!

=head1 NAME

[[full-module-name]] - Perl extension for blah blah blah

=head1 SYNOPSIS

  use [[full-module-name]];
  blah blah blah

=head1 DESCRIPTION

Stub documentation for [[full-module-name]], created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head2 EXPORT

None by default.


=head1 AUTHOR

[[author]], E<lt>[[author-email]]<gt>

=head1 EXPORTED PACKAGE GLOBALS
=over 4
=cut

# exported package globals go here

= item $Var1

$Var1 is ...

=cut

#our $Var1;

= item %Hashit

%Hashit is ...

=cut

#our %Hashit;


=back

=head1 NON-EXPORTED PACKAGE GLOBALS
=over 4
=cut

= item @nonExported

@nonExported is ...

=cut

#our @nonExported;

=back

=cut

# initialize package globals, first exported ones
#$Var1   = '';
#%Hashit = ();

# then the others (which are still accessible as $Some::Module::stuff)
#$stuff  = '';
#@more   = ();

# all file-scoped lexicals must be created before
# the functions below that use them.
# file-private lexicals go here
#my $priv_var    = '';
#my %secret_hash = ();

# here's a file-private function as a closure,
# callable as &$priv_func;  it cannot be prototyped.
#my $priv_func = sub {
#    # stuff goes here.
#};

=head1 METHODS
=over 4
=cut

# make all your functions, whether exported or not;
# remember to put something interesting in the {} stubs
=item func1 $x,$y

The func1 subroutine ...

=cut

# no prototype
#sub func1 {
#    my ($x,$y) = @_;
#
#}

#sub func2()    {}    # proto'd void
#sub func3($$)  {}    # proto'd to 2 scalars
# this one isn't exported, but could be called!
#sub func4(\%)  {}    # proto'd to 1 hash ref

=back
=cut

END { }       # module clean-up code here (global destructor)
## YOUR CODE GOES HERE
1;  # don't forget to return a true value from the file

=head1 SEE ALSO

L<perl>.

=cut

1;
__END__
