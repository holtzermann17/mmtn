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

// Implements the listener thread, which is responsible for listening
// on a port for new connections. There can be more than one of these,
// if you want to listen on more than one port.

#ifndef MMTN_LISTENER_HPP
#define MMTN_LISTENER_HPP

#include <pthread.h>

namespace mmtn
{
	class Listener
	{
	public:
		Listener(int port);
		~Listener();
		int get_port_number() { return m_port; }
	private:
		// The thread callback.
		static void *run_listener_thread(void *listener);

		pthread_t m_listener_thread;
		int m_port;
		int m_sock;

		// The size of the connection backlog, passed to listen().
		static const int listen_backlog = 8;
	};
}

#endif
