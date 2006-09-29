// Monster Mountain Server
// Copyright (C) 2006 Nick Thomas <jesuswaffle@gmail.com>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; only version 2 of the
// License is applicable.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

#include "mmtn.hpp"
#include "main.hpp"
#include "logger.hpp"
#include "listener.hpp"
#include <stdio.h>

using namespace mmtn;

int main(int argc, char **argv)
{
	static FILE *streams[2];
	streams[0] = stderr;
	logger = new Logger(streams);
	
	try {
		Listener *listener = new Listener(4141);
		printf("Press ENTER to stop listening.\n");
		getchar();
		delete listener; 
	} catch (FatalError *e) {
		return 1;
	}

	return 0;
}
