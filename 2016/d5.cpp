#include <stdio.h>
#include <string>
#include "lib/MD5.h"

std::string door_id = "cxdnnyjw";

int main() {
	std::string checksum;
	std::string password;
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
			// append found char
			password += checksum[5];
			printf("password: %s\n", password.data());
			if (password.length() > 7) {
				printf("Final password is: %s\n", password.data());
				return(0);
			}
		}

		index++;
	}
		
	return 0;
}
