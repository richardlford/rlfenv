=item $suite_expected

$suite_expected is a reference to a hash, keyed on docvsu caption, of
the expected results for that part of the test.

=cut

my $suite_expected = {};

fill_suite_expected();

print 'New suite_expected = ' . Dumper($suite_expected);

=head2 fill_suite_expected 

The fill_suite_expected subroutine is used to fill the %suite_expected
hash. This is done in a separate subroutine so the meat of the test
can appear at the front of the file, and the supporting data at the
end in this routine.

=cut

sub fill_suite_expected {

    # Expected results.
    $suite_expected =
      {} # Initial stub value.
    ;
}

