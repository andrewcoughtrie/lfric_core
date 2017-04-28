##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

INITIAL_lfric_VERSION    = v1.1
FINAL_lfric_VERSION      = HEAD
INITIAL_psyclone_VERSION = vn1.3.1
FINAL_psyclone_VERSION   = HEAD

lfric_REPOSITORY_URL   = https://code.metoffice.gov.uk/svn/lfric/LFRic
psyclone_REPOSITORY_URL = https://code.metoffice.gov.uk/svn/lfric/PSyclone

Q ?= @

REPOSITORY_URL  = $($*_REPOSITORY_URL)
INITIAL_VERSION = $(INITIAL_$*_VERSION)
FINAL_VERSION   = $(FINAL_$*_VERSION)

gource-$(FINAL_lfric_VERSION).mp4: gource/combined.txt gource/lfric-captions.txt
	$(warning The 'gource' visualisation tool takes frames straight from rendered output.)
	$(warning If you obscure the window during rendering frames will be incomplete.)
	gource -1280x720 -r 25 --multi-sampling --title LFric \
	    --user-image-dir avatars \
	    --key -caption-file gource/lfric-captions.txt \
	    --hide mouse,progress,usernames,filenames,dirnames \
	    --auto-skip-seconds 2 --seconds-per-day 1 --file-idle-time 30 -o - \
	    $< \
	| ffmpeg -y -r 25 -f image2pipe -vcodec ppm -i - \
	         -c:v libx264 -preset slow -crf 22 -profile:v high -level 4.2 $@

gource/combined.txt: gource/lfric-log.txt gource/psyclone-log.txt
	$(info Combining Dynamo and PSyclone logs)
	$(Q)cat $^ | sort -n > $@

gource/%-log.txt: gource/%-log.xml
	$(info Converting $* log)
	$(Q)gource --output-custom-log $@ $<

gource/%-captions.txt: gource/%-head.txt
	$(info Extraction $* captions)
	$(Q)tools/encaptor $(REPOSITORY_URL)/trunk $@

.PRECIOUS: gource/%-log.xml
gource/%-log.xml: gource/%-head.txt
	$(info Obtaining $* log $(shell cat $<))
	$(Q)svn log -r $(shell cat $<) --xml --verbose --quiet $(REPOSITORY_URL)/trunk > $@

.PRECIOUS: gource/%-head.txt
gource/%-head.txt: ALWAYS
	$(info Determining revision of $*)
	$(Q)tools/headaliser -release $(INITIAL_VERSION):$(FINAL_VERSION) $(REPOSITORY_URL) $@

ALWAYS:

.PHONY: clean
clean:
	-rm -rf gource/*
