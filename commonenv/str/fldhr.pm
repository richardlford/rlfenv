         do{

=item [[field-name]]_tab : Hash([[element-type]])

[[field-name]]_tab is zapp

=cut

           [[field-name]]_tab => Hash([[element-type]])
         },
         do{

=item [[field-name]]_tab_ref : Method()

[[field-name]]_tab_ref is a method which returns a reference to the
[[field-name]]_tab member value.

=cut

           '&[[field-name]]_tab_ref' =>
             q{
                 return $self->{'[[field-name]]_tab'};
             }
         },
         # next-member-zapp
