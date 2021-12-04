#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define BITS 12
#define SIZE 1000

int bits_to_int(char *buffer) {
	int num = 0;
	for(int i = 0; i < BITS; i++) {
		num |= (buffer[i] - '0') << (BITS - 1 - i);
	}
	return num;
}

int main(int argc, char **argv) {
	if (argc<2) {
		fprintf(stderr, "Need a filename\n");
		exit(1);
	}

	FILE *f = fopen(argv[1], "r");
	char buffer[BITS];
	char bits[SIZE][BITS] = {0};
	int count[BITS] = {0}, line=0, i, j;

	while(!feof(f)) {
		fscanf(f, "%s\n", buffer);
		memcpy(bits[line++], buffer, BITS);
		for(i=0;i<BITS; i++) {
			count[i] += (buffer[i] == '1')?  1 : -1;
		}
	}
	int gamma = 0, epsilon;
	for(i=0; i < BITS; i++){
		gamma |= (count[i] >= 0 ? 1 : 0) << (BITS - 1 - i);
	}
	epsilon = (~gamma) & ((1 << BITS) - 1);
	printf("Part 1: %d\n", gamma * epsilon);

	char as_far[SIZE][BITS];
	int as_far_size=SIZE;
	// calculate oxygen
	memcpy(as_far, bits, BITS*SIZE);
	for(i = 0; i < BITS; i++) {
		//determine most common bit
		int ones = 0;
		for(j = 0; j < as_far_size; j++) {
			ones += (as_far[j][i] - '0') << 1;
			if(ones >= as_far_size) break;
		}
		char criteria = (ones >= as_far_size)? '1' : '0';
		int c = 0;
		for( j=0; j<as_far_size; j++) {
			if(as_far[j][i] == criteria) memmove(as_far[c++], as_far[j], BITS);
		}
		as_far_size = c;
	}
	int oxygen = bits_to_int(as_far[0]);

	memcpy(as_far, bits, BITS*SIZE);
	as_far_size = SIZE;
	for(i = 0; i < BITS; i++) {
		//determine most common bit
		int ones = 0;
		for(int j = 0; j < as_far_size; j++) {
			ones += (as_far[j][i] - '0') << 1;
			if(ones >= as_far_size) break;
		}
		char criteria = (ones >= as_far_size)? '0' : '1';
		int c = 0;
		for( j=0; j<as_far_size; j++) {
			if(as_far[j][i] == criteria) memmove(as_far[c++], as_far[j],BITS);
		}
		as_far_size = c;
	}
	int scrubber = bits_to_int(as_far[0]);
	printf("oxygen: %d\tscrubber: %d\n", oxygen, scrubber);
	printf("Part 2: %d\n", oxygen * scrubber);

}
