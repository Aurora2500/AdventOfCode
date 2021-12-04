#include<stdio.h>
#include<stdlib.h>

int main(int argc, char **argv) {
	if(argc < 2) {
		fprintf(stderr, "Need a filename\n");
		exit(1);
	}

	FILE *f = fopen(argv[1], "r");
	char action[20];
	int ammount, x=0, y1=0, y2=0, aim=0;
	while(!feof(f)) {
		fscanf(f, "%s %d\n", action, &ammount);
		switch(*action) {
			case 'f':
				x += ammount;
				y2 += aim * ammount;
				break;
			case 'd':
				y1 += ammount;
				aim += ammount;
				break;
			case 'u':
				y1 -= ammount;
				aim -= ammount;
				break;
		}
	}
	fclose(f);
	printf("Part 1: %d\n", x*y1);
	printf("Part 2: %d\n", x*y2);
}
