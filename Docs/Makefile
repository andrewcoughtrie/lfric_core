DOXYGEN ?= doxygen

.PHONY: api
api:
	$(DOXYGEN) Doxyfile

.PHONY: gource
gource:
	$(info *** The 'gource' visualisation tool takes frames straight from rendered output.)
	$(info *** If you obscure the window during rendering frames will be incomplete.)
	gource -1280x720 -r 30 --multi-sampling --title Dynamo \
	       --user-image-dir gource --caption-file gource/captions.txt \
	       --key --hide root,mouse,progress -o - \
	       | ffmpeg -y -r 30 -f image2pipe -vcodec ppm -i - -c:v mpeg4 \
	       -s 1280x720 -r 30 -pix_fmt yuv420p -g 15 -b:v 2000k \
	       -maxrate 4000k -minrate 1000k -bufsize 5000k -packetsize 4k \
	       -muxrate 5000k dynamo-gource.mp4

.PHONY: clean
clean:
	-rm -rf docs/api/*
