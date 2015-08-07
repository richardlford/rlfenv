# [[test-name]] test.
$(MSILOBJDIR)\[[test-name]].exe: [[test-name]].cs $(MSILOBJDIR)
        $(BC) $(BCFLAGS_START) /ilonly /unsafe /midori /outcs:$@ [[test-name]].cs ATestCase.cs

$(OBJDIR)\[[test-name]].exe: $(MSILOBJDIR)\[[test-name]].exe
        $(BC) $(BCFLAGS_START) $(LOG) /midori $(MSILOBJDIR)\[[test-name]].exe $(BCFLAGS_END)

