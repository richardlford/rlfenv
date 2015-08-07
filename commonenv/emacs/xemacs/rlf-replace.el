;;; rlf-replace.el

(defun query-replace-region (from-string to-string start end &optional arg)
  "Replace some occurrences of FROM-STRING with TO-STRING.
 As each match is found, the user must type a character saying
 what to do with it.  For directions, type \\[help-command] at that time.
 Replacement is only done for matches in region start to end 
 (interactively the region between point and mark). 

 Preserves case in each replacement if  case-replace  and  case-fold-search
 are non-nil and FROM-STRING has no uppercase letters.
 Third arg DELIMITED (prefix arg if interactive) non-nil means replace
 only matches surrounded by word boundaries."
  (interactive "sRegion Query replace: 
sRegion Query replace %s with: 
r
P")
  (save-excursion
    (perform-replace-region
     from-string to-string t nil arg start end))
  )

(defun query-replace-regexp-region (regexp to-string start end &optional arg) "\
Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.
Replacement is only done for matches in region start to end 
(interactively the region between point and mark). 

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\=\\<n> means insert what matched <n>th \\(...\\) in REGEXP." (interactive "sRegion Query replace regexp: 
sRegion Query replace regexp %s with: 
r
P") (byte-code "ÆˆŠÇ	ÂÂ&)ˆÈÉ!‡" [regexp to-string t arg start end nil perform-replace-region message "Done"] 8))

(defun replace-string-region (from-string to-string start end &optional delimited) "\
Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries." (interactive "sRegion Replace string: 
sRegion Replace string %s with: 
r
P") (byte-code "ÂˆŠÆ	ÂÂ&)ˆÇÈ!‡" [from-string to-string nil delimited start end perform-replace-region message "Done"] 8))

(defun replace-regexp-region (regexp to-string start end &optional delimited) "\
Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if case-replace and case-fold-search
are non-nil and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\=\\<n> means insert what matched <n>th \\(...\\) in REGEXP." (interactive "sRegion Replace regexp: 
sRegion Replace regexp %s with: 
r
P") (byte-code "ÂˆŠÇ	ÂÃ&)ˆÈÉ!‡" [regexp to-string nil t delimited start end perform-replace-region message "Done"] 8))

(defun perform-replace-region (from-string to-string query-flag regexp-flag delimited-flag start end)
  
  )

(defun tags-replace (from to &optional delimited) "\
Replace-regexp FROM with TO through all files listed in tag table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (C-G or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable tags-file-name." (interactive "sTags replace (regexp): 
sTags query replace %s by: 
P") (byte-code "ÂˆÆÇÈ	ÂÃFDÉÊË	ÂÃ??&DE‰ˆÌÃ!‡" [tags-loop-form from nil t to delimited and save-excursion re-search-forward not list perform-replace tags-loop-continue] 10))

(defun query-interchange (from-string to-string &optional delimited) "\
Interchange some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserves case in each interchange if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means interchange
only matches surrounded by word boundaries." (interactive "sQuery interchange: 
sQuery interchange %s with: 
P") (byte-code "ÄˆÅ	Âed&ˆÆÇ!‡" [from-string to-string t delimited nil perform-interchange-region message "Done"] 7))

(defun interchange-string (from-string to-string &optional delimited) "\
Interchange occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means interchange
only matches surrounded by word boundaries." (interactive "sInterchange string: 
sInterchange string %s with: 
P") (byte-code "ÂˆÄ	Âed&ˆÅÆ!‡" [from-string to-string nil delimited perform-interchange-region message "Done"] 7))

(defun query-interchange-region (from-string to-string start end &optional arg) "\
Interchange some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.
Interchange is only done for matches in region start to end 
(interactively the region between point and mark). 

Preserves case in each interchange if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means interchange
only matches surrounded by word boundaries." (interactive "sRegion Query interchange: 
sRegion Query interchange %s with: 
r
P") (byte-code "ÆˆŠÇ	Â&)ˆÈÉ!‡" [from-string to-string t arg start end nil perform-interchange-region message "Done"] 7))

(defun interchange-string-region (from-string to-string start end &optional delimited) "\
Interchange occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means interchange
only matches surrounded by word boundaries." (interactive "sRegion Interchange string: 
sRegion Interchange string %s with: 
r
P") (byte-code "ÂˆŠÆ	Â&)ˆÇÈ!‡" [from-string to-string nil delimited start end perform-interchange-region message "Done"] 7))

(defun perform-interchange-region (from-string to-string query-flag delimited-flag start end)
  (let (
	(lastrepl nil)
	(end-marker copy-marker end)
	(matched-string nil)
	(new-string nil)
	(literal t)
	(keep-going t)
	(nocasify
	 (not (and
	       case-fold-search
	       case-replace
	       (string-equal from-string (downcase from-string))
	       (string-equal to-string (downcase to-string)))))
	)
    (if delimited-flag
	(setq search-string
	      (concat "\\(\\b"
		      (regexp-quote from-string)
		      "\\b\\|\\b"
		      (regexp-quote to-string)
		      "\\b\\)"))
      (setq search-string
	    (concat "\\("
		    (regexp-quote from-string)
		    "\\|"
		    (regexp-quote to-string)
		    "\\)")))
    (push-mark)
    (push-mark)
    (goto-char start)

    (while (and keep-going
		(not (eobp))
		(< (point) end-marker)
		(progn
		  (set-mark (point))
		  (re-search-forward
		   search-string end-marker t)))
      (if (eq lastrepl (point))
	  (forward-char 1)
	(undo-boundary)
	(setq matched-string
	      (buffer-substring (match-beginning 0)
				(match-end 0)))
	(if (string-equal matched-string from-string)
	    (setq new-string to-string)
	  (setq new-string from-string))
	(if (not query-flag)
	    (replace-match new-string nocasify literal) ;;goto 33
	  (let (done replaced)
	    
186:9	constant  nil
187	constant  nil
188	varbind	  replaced
190	varbind	  done
192:10	varref	  done
194	not	  
195	goto-if-nil-else-pop 32
198	constant  match-data
199	call	  0
200	constant  (concat "Query replacing " matched-string " with " new-string ".\n\n" (substitute-command-keys query-replace-help))
201	varbind	  help-form
203	varbind	  data
205	varref	  help-char
207	dup	  
208	varset	  char
210	discard	  
211:11	varref	  char
213	varref	  help-char
215	eqlsign	  
216	goto-if-nil-else-pop 13
219	constant  message
220	constant  "Query interchanging %s with %s: "
221	varref	  from-string
222	varref	  to-string
223	call	  3
224	discard	  
225	read-char-OBSOLETE 
226	dup	  
227	varset	  char
229	discard	  
230	varref	  char
232	constant  63
233	eqlsign	  
234	goto-if-nil-else-pop 12
237	varref	  help-char
239	varset	  unread-command-char
241	varref	  help-char
243	dup	  
244	varset	  char
246:12	discard	  
247	goto	  11
250:13	discard	  
251	constant  store-match-data
252	varref	  data
254	call	  1
255	unbind	  2
256	discard	  
257	varref	  char
259	constant  27
260	eqlsign	  
261	goto-if-not-nil-else-pop 14
264	varref	  char
266	constant  113
267	eqlsign	  
268:14	goto-if-nil 15
271	constant  nil
272	dup	  
273	varset	  keep-going
274	discard	  
275	constant  t
276	dup	  
277	varset	  done
279	goto	  31
282:15	varref	  char
284	constant  94
285	eqlsign	  
286	goto-if-nil 16
289	constant  mark
290	call	  0
291	goto-char 
292	discard	  
293	constant  t
294	dup	  
295	varset	  replaced
297	goto	  31
300:16	varref	  char
302	constant  32
303	eqlsign	  
304	goto-if-not-nil-else-pop 17
307	varref	  char
309	constant  121
310	eqlsign	  
311:17	goto-if-nil 19
314	varref	  replaced
316	goto-if-not-nil-else-pop 18
319	constant  replace-match
320	varref	  new-string
322	varref	  nocasify
323	varref	  literal
325	call	  3
326:18	discard	  
327	constant  t
328	dup	  
329	varset	  done
331	goto	  31
334:19	varref	  char
336	constant  46
337	eqlsign	  
338	goto-if-nil 21
341	varref	  replaced
343	goto-if-not-nil-else-pop 20
346	constant  replace-match
347	varref	  new-string
349	varref	  nocasify
350	varref	  literal
352	call	  3
353:20	discard	  
354	constant  nil
355	dup	  
356	varset	  keep-going
357	discard	  
358	constant  t
359	dup	  
360	varset	  done
362	goto	  31
365:21	varref	  char
367	constant  44
368	eqlsign	  
369	goto-if-nil 23
372	varref	  replaced
374	not	  
375	goto-if-nil-else-pop 22
378	constant  replace-match
379	varref	  new-string
381	varref	  nocasify
382	varref	  literal
384	call	  3
385	discard	  
386	constant  t
387	dup	  
388	varset	  replaced
390:22	goto	  31
393:23	varref	  char
395	constant  33
396	eqlsign	  
397	goto-if-nil 25
400	varref	  replaced
402	goto-if-not-nil-else-pop 24
405	constant  replace-match
406	varref	  new-string
408	varref	  nocasify
409	varref	  literal
411	call	  3
412:24	discard	  
413	constant  t
414	varset	  done
416	constant  nil
417	dup	  
418	varset	  query-flag
420	goto	  31
423:25	varref	  char
425	constant  127
426	eqlsign	  
427	goto-if-not-nil-else-pop 26
430	varref	  char
432	constant  110
433	eqlsign	  
434:26	goto-if-nil 27
437	constant  t
438	dup	  
439	varset	  done
441	goto	  31
444:27	varref	  char
446	constant  12
449	eqlsign	  
450	goto-if-nil 28
453	constant  recenter
456	constant  nil
457	call	  1
458	goto	  31
461:28	varref	  char
463	constant  18
466	eqlsign	  
467	goto-if-nil 29
470	constant  store-match-data
471	constant  match-data
472	call	  0
473	save-excursion 
474	constant  recursive-edit
477	call	  0
478	unbind	  1
479	discard	  
480	call	  1
481	goto	  31
484:29	varref	  char
486	constant  23
489	eqlsign	  
490	goto-if-nil 30
493	constant  delete-region
496	constant  match-beginning
497	constant  0
498	call	  1
499	constant  match-end
500	constant  0
501	call	  1
502	call	  2
503	discard	  
504	constant  store-match-data
505	constant  match-data
506	call	  0
507	save-excursion 
508	constant  recursive-edit
511	call	  0
512	unbind	  1
513	discard	  
514	call	  1
515	discard	  
516	constant  t
517	dup	  
518	varset	  replaced
520	goto	  31
523:30	constant  nil
524	dup	  
525	varset	  keep-going
526	discard	  
527	varref	  char
529	dup	  
530	varset	  unread-command-char
532	discard	  
533	constant  t
534	dup	  
535	varset	  done
537:31	discard	  
538	goto	  10
541:32	unbind	  2
542:33	discard	  
543	point	  
544	dup	  
545	varset	  lastrepl
547:34	discard	  
548	goto	  4
551:35	discard	  
552	constant  pop-mark
555	call	  0
556	discard	  
557	varref	  keep-going
558	unbind	  7
560	return	  

  (byte-code "	… 
… ÙÚ!\"… ÙÚ!\"?ÆÆÍÍÛ!Í
	ƒC ÜÝÞ!ßÞ!à%‰‚Q ÜáÞ!âÞ!ã%‰ˆä ˆä ˆbˆ…w m?…w `
W…w å`!ˆæ
Æ#…'`=ƒ‡ çè!‚#é ˆêëì!íì!\"‰	ˆÙ	\"ƒ¥ ‰‚© ‰ˆ?ƒº î#‚ÍÍ?…ï ð‰ˆU…ú ñò#ˆr‰ˆóU…ö ‰ˆ‚Ó ˆô!*ˆõU†öUƒ