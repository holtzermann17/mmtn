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

// The logging system. Each log message has a log level.

#ifndef MMTN_LOGGER_HPP
#define MMTN_LOGGER_HPP

#include <stdio.h>

namespace mmtn
{
	typedef enum {
		// Indicates a noteworthy system event.
		lvl_info = 0,
		// Indicates that there could be a problem, but
		// nothing bad has happened yet.
		lvl_warning = 1,
		// Indicates that something went wrong, but it's
		// recoverable.
		lvl_error = 2,
		// Indicates an assertion failure. Signalling one of
		// these causes an exception to be thrown that causes
		// the thread to exit.
		lvl_assert = 3,
		// Indicates an unrecoverable error. Also throws an
		// exception, as described above.
		lvl_fatal = 4
	} log_level;

	// Asserts that the given condition is true, signalling an
	// error if it is not.
#ifdef NDEBUG
#define mmtn_assert(cond)
#else
#define mmtn_assert(cond) \
  ((cond) ? 0 : logger->assert_fail("cond", __LINE__, __FILE__))
#endif
	
	class Logger
	{
	public:
		// Creates a new logger, which sends its output to all
		// of the given streams. (The list is terminated with
		// a NULL pointer.)
		Logger(FILE **streams) { m_streams = streams; }
		
		// Writes a log message with printf-style arguments.
		void log(log_level level, char *format, ...)
		// The following magic makes the compiler type-check
		// the format arguments under GCC.
#ifdef __GNUC__
			__attribute__ ((format (__printf__, 3, 4)))
#endif
		;

		// The runtime part of the mmtn_assert()
		// macro. Returns int so that mmtn_assert() typechecks.
		int assert_fail(char *expr, int line, char *file); 
	private:
		FILE **m_streams; 
		static char *level_tags[];
	};

	// The exception type thrown by fatal errors.
	class FatalError
	{
	public:
		FatalError() { }
	};

	// The static logger instance.
	extern Logger *logger;
}

#endif
