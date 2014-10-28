These tests are used during development to look for C memory problems by running
them using:

    R -d valgrind --vanilla < matchcall.valgrind.R

They are copies of the `unitizer` tests with all the actual `unitizer` stuff
removed and `gctorture` added.
