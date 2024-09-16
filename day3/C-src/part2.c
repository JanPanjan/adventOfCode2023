#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 140
#define MAX_LENGTH 256

char *readLine(FILE *file);
// makeDataMatrix
// addToMatrix
// isInMatrix
// getNumLen
// getNumAroundStar
// getNum
// isSymbol
// isDigit

int main() {
	FILE *file = fopen("testData.txt", "r");
	return 0;
}

char *readLine(FILE *file) {
	// buffer bo hranil prebrano vrstico
	char buffer[MAX_LENGTH];

	// če vrne NULL, smo na koncu datoteke, ali pa je error
	if (fgets(buffer, sizeof(buffer), file) == NULL) {
		return NULL;
	}

	// dinamično alociramo spomin za vrstico
	char *line = malloc(strlen(buffer) + 1);

	if (line == NULL) {
		printf("readLine: Memory allocation error!\n");
	 return NULL;
	}

	// kopiramo line iz bufferja v alociran spomin
	strcpy(line, buffer);
	return line;
}
