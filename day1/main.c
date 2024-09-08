#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
	/*
* combine first and last digit on each line to form a single two digit number (glej example)
* 	- prva in zadnja
* 	- če je ena, jo vzami dvakrat
* vrni sum dvomestnih števil
* kako preberem datoteko v C-ju...
* got it
*/

	FILE *data = fopen("data.txt", "r");

	if (data == NULL) {
		printf("Can't open file. File does not exist.\n");
		return 1;
	}

	char buffer[100];
	int calibrationData = 0;
	int *calibrationData_p = &calibrationData;

	// gledamo vsako vrstico posebej
	while (fgets(buffer, 100, data) != NULL) {
		int SIZE = strlen(buffer)-1; //zaradi '\n'
		// števila niso nikoli enaka 0
		// (did a quick ctrl-f)
		int prvoSt     = 0;
		int drugoSt    = 0;
		int *prvoSt_p  = &prvoSt;
		int *drugoSt_p = &drugoSt;

		// da seštejemo števili, morata biti najbolj oddaljeni med sabo
		// gremo iz vsake strani
		for (int i = 0; i < SIZE; i++) {
			char znakLevo = buffer[i];
			char znakDesno = buffer[SIZE-i-1];

			// ko najdemo število, preverimo, če že imamo število
			// -48 ker imamo chars oziroma ASCII vrednosti
			if (isdigit(znakLevo) && prvoSt == 0) {
				*prvoSt_p = znakLevo - 48;
			}
			if (isdigit(znakDesno) && drugoSt == 0) {
				*drugoSt_p = znakDesno - 48;
			}

			// ko najdemo naši števili, exitamo loop
			if (prvoSt != 0 && drugoSt != 0) {
				break;
			}

			// če najdemo eno število, moramo najti samo še drugo
			// če pride desni do tega indexa, se shrani v drugo število isto število
			// exitamo loop
		}

		// dodamo calibration data
		// naredimo dvomestno število
		*calibrationData_p += 10 * *prvoSt_p + *drugoSt_p;
	}
	fclose(data);

	int COMPLETE_DATA = *calibrationData_p;

	FILE *odgovor = fopen("odgovor.txt", "w");
	fprintf(odgovor, "sum of calibration data: %d\n", COMPLETE_DATA);
	fclose(odgovor);

	return 0;
}
