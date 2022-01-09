#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cstdio>

using namespace std;
int main() {
	vector<string> lines(1092);
	ifstream infile("d4_input.txt");

	// read file line by line
	int i=0;
	while (getline(infile, lines[i]))
	{
		i++;
	}

	int idsum = 0;
	for(int i=0; i<1091; ++i) {
		char name[255] = { 0 };
		char checksum[5] = { 0 };
		int id = 0;
		int j=0;
		while(1) {
			if(lines[i][j] > 47 && lines[i][j] < 58) break;  // char is a number 47->'0'
			name[j] = lines[i][j];
			j++;
		}

		// convert susbstr to int with stoi
		id = stoi(lines[i].substr(j, 3));

		// write checksum in checksum char array
		// continuing from j-> 000[a <- j+4
		for (int f=0; f<5; ++f) {
			checksum[f] = lines[i][j+4+f];
		}

		/*
		// counted letters
		char letter[26];
		// nr of occurences of letter in name
		int  anzahl[26] = { 0 };
		int  zaehler = -1;
		for (int start=0; start<j; ++start) {
			if (name[start] == '-') continue;
			zaehler++;
			// store letter were counting in array
			letter[zaehler] = name[start];
			// inc count
			anzahl[zaehler]++;

			for (int comp=start+1; comp<j; ++comp) {
				// letters match
				// increase count and replace with -
				// so we dont count it next time
				if (name[start] == name[comp]) {
					anzahl[zaehler]++;
					name[comp] = '-';
				}
			}
		}

		for (int k =0; k<5; ++k) {
			// find biggest nr in anzahl array
			int max_i = 0;
			// assume first is biggest and start comparing with next one
			for (int i=1; i<=zaehler; ++i) {
				if (anzahl[i] > anzahl[max_i]) max_i = i;
				else if (anzahl[i] == anzahl[max_i]) {
					// if its a tie -> letter that comes first in alphabet is chosen
					if (letter[i] < letter[max_i])
						max_i = i;
				}
			}
			// set max to 0 so we dont find it again when we continue
			// with next smaller nr
			anzahl[max_i] = 0;

			// invalid if current max letter doesnt match letter in
			// checksum at current pos k
			if (checksum[k] != letter[max_i]) break;
			
			// if we checked all previous biggest nrs
			// -> valid -> inc idsum
			if (k==4) idsum += id;
		}
		*/
		
		char decrypted[255] = { 0 };
		for (int i=0; i<j; ++i) {
			if (name[i] == '-') {
				decrypted[i] = ' ';
				continue;
			}
			// pos of char in alphabet relative to 'a'
			int pos = (int)name[i] - 97;
			// abs/rel to a position of decrypted letter in alphabet
			char letter_decrypted = (pos + id) % 26;
			// convert back to normal char
			letter_decrypted += 97;

			decrypted[i] = letter_decrypted;
		}
		// checks for complete str match
		//if (strcmp(decrypted, "northpole") == 0) printf("ID of north pole is %d", id);
		// checks if susbstr (here north) is present -> returns pointer to pos in str if true
		if(strstr(decrypted, "north") != NULL) {
			printf("ID of north pole is: %d\n", id);
		}
	}
	printf("Sum is: %d\n", idsum);
}
