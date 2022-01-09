#include <stdio.h>
#include <string>
#include "lib/MD5.h"

std::string door_id = "cxdnnyjw";

int main() {
	std::string checksum;
	std::string password = "________";
	MD5 md5;
	unsigned int index = 0;

	// use char array here since we need to format it with snprintf
	// and that doesnt work with string (wed have to convert to char*
	// format and then convert back)
	char door_id_index[33];
	// printf door_id with appended index to door_id_index
	while (index < 0xffffffff) {
		snprintf(door_id_index, 33, "%s%u", door_id.data(), index);
		checksum = md5.getMD5(door_id_index, strlen(door_id_index));
		if (checksum.substr(0, 5) == "00000") {
			printf("checksum: %s\n", checksum.data());
			// ignore invalid pos
			if ((checksum[5] >= '0') && (checksum[5] < '8')) {
				// get int from 6th char in cheksum str
				// by substracting the value of '0'
				int pos = checksum[5] - '0';
				// only use first result
				if (password[pos] == '_') password[pos] = checksum[6];
				printf("password: %s\n", password.data());
				// make sure no _ left in password
				// returns npos if not found else position
				if (password.find('_') == std::string::npos) {
					printf("Final password is: %s\n", password.data());
					return(0);
				}
			}
		}

		index++;
	}
		
	return 0;
}
