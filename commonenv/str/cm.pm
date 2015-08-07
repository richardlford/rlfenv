=head2 [[cvs-method]] $self,$global_opts_ref,$command_opts_ref,$args_ref returns CVSI::[[cap-cvs-method]]Command::Output

Interface to cvs command [[cvs-method]].
[[description]]

The method parameters follow our general scheme of self reference, ref to
global option hash, ref to command-specific option hash, and ref to
argument array.

Returns object of type CVSI::[[cap-cvs-method]]Command::Output

=cut

sub [[cvs-method]] {
    my CVSI $self = shift;
    my ($global_opts_ref,$command_opts_ref,$args_ref) = @_;
    
    my CVSI::Command::Output $result =
        $self -> exec_cvs_command('[[cvs-method]]',
                                  $global_opts_ref,
                                  $command_opts_ref,
                                  $args_ref
                                 );
    
    bless($result, 'CVSI::[[cap-cvs-method]]Command::Output');
                    
    return $result;
}
