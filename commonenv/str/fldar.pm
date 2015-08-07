         do{

=item [[field-name]]_ray : Array([[element-type]])

[[field-name]]_ray is zapp

=cut

           [[field-name]]_ray => Array([[element-type]])
         },
         do{

=item [[field-name]]_ray_ref : Method()

[[field-name]]_ray_ref is a method which returns a reference to the
[[field-name]]_ray member value.

=cut

           '&[[field-name]]_ray_ref' =>
             q{
                 return $self->{'[[field-name]]_ray'};
             }
         },
         # next-member-zapp
