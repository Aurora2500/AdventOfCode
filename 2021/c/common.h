#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
	size_t size;
	char *begin;
} SV;

SV from_owned(char *data) {
	return (SV){.begin=data,.size=strlen(data)};
}

int chop_int(SV *sentence) {
	
}

void trim_sv(SV *sentence, int ammount) {
	sentence->begin += ammount;
	sentence->size  -= ammount;
}

SV chop_word(SV *sentence) {
	int i = 0;
	while(i <= sentence->size && sentence->begin[i] != ' ') i++;
	SV word = (SV){.begin=sentence->begin, .size=i};
	sentence->begin += i;
	sentence->size  -= i;
}

typedef struct {
	FILE *file
	char *buffer;
	size_t size;
	size_t cap;
} LineIterator;

LineIterator open_iter(char *filename) {
	FILE *f = fopen(filename, "r");
	return (LineIterator){
		.file=f,
		.buffer=malloc(1024),
		.size=0,
		.cap=1024
	};
}

char *get_line(LineIterator *iter) {
	if((iter->size == 0) && (iter->f == NULL)) {
		return NULL;
	}
	if(iter->size == 0) {
		iter->size = fread(iter->buffer, 1, iter->cap, iter->f);
		if(eof(iter->f)) {
			fclose(iter->f);
			iter->f = NULL;
		}
	}
	size_t c = 0, t = 0;
	char *line;
	while(c < iter->size && iter->buffer[c] != "\n") c++;

	if(c == iter->size) {
		
	} else {
		line
	}
}
