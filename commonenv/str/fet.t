my  Filter::Deeply $filter = Filter::Deeply->new();

fill_expected_tab();

$filter->print_new_expected_if_failed();

=head2 fill_expected_tab 

The fill_expected_tab subroutine is used to fill the
$filter->expected_tab hash. This is done in a separate subroutine so
the meat of the test can appear at the front of the file, and the
supporting data at the end in this routine.

=cut

sub fill_expected_tab {

    # Expected results.
    my $expected = {};
    $filter->expected_tab($expected);
}

