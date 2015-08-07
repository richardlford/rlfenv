is_deeply(['[[data]]' =~ /@{[[[pattern-name]]]}/],
          [qw([[value]])],
         'good [[pattern-name]]');

is_deeply(['stuff' =~ /@{[[[pattern-name]]]}/],
          [],
         'bad [[pattern-name]]');
