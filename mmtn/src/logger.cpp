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
#include "logger.hpp"
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>

using namespace mmtn;

char *Logger::level_tags[] = {
	"[info] ", "[warn] ", "[ERROR] ", "[ASSERT] ", "[FATAL] "
};

Logger *mmtn::logger;

void Logger::log(log_level level, char *format, ...)
{
	va_list argp;
	char *tag = level_tags[level];

	for (int i = 0; m_streams[i]; i++) {
		if (fputs(tag, m_streams[i]) < 0) {
			goto IO_ERROR;
		}
		
		va_start(argp, format);
		
		if (vfprintf(m_streams[i], format, argp) < 0) {
			va_end(argp);
			goto IO_ERROR;
		}
		
		va_end(argp);

		continue;
	IO_ERROR:
		fprintf(stderr, "I/O error attempting to write to log "
			"(file descriptor %i): %s\n",
			fileno(m_streams[i]), strerror(errno));
	}

	if (level == lvl_fatal) {
		throw new FatalError();
	}
}

int Logger::assert_fail(char *expr, int line, char *file)
{
	log(lvl_assert, "assertion failure '%s' at %s line %i",
	    expr, file, line);

	// Unreachable, but the compiler doesn't know that...
	return 0;
}
