         do{

=item &[[field-name]]_ref : Method()

&[[field-name]]_ref is a method which returns a reference to the
[[field-name]] member.

=cut

           '&[[field-name]]_ref' =>
             q{
                 return $self->{'[[field-name]]'};
             }
         },
         # next-member-zapp
