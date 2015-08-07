chomp($line = &$get_line());
($line =~ /^[[pattern]]: (.*)$/) or die "Missing [[pattern]] in log";
$flvi->[[field]]($1);
