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
#include "listener.hpp"
#include "logger.hpp"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

using namespace mmtn;

Listener::Listener(int port)
{
	m_port = port;
	m_sock = socket(PF_INET, SOCK_STREAM, 0);
	
	if (m_sock < 0) {
		logger->log(lvl_fatal, "Could not make socket: %s\n",
			    strerror(errno));
	}	

	struct sockaddr_in addr;

	addr.sin_family = AF_INET;
	addr.sin_port = htons(m_port);
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	memset(addr.sin_zero, 0, 8);

	if (bind(m_sock, (struct sockaddr *)&addr,
		 sizeof(struct sockaddr)) < 0) {
		logger->log(lvl_fatal, "Could not bind to port %i: %s\n",
			    m_port, strerror(errno));
	}

#ifdef __solaris__
	char yes = '1';
#else
	int yes = 1;
#endif

	if (setsockopt(m_sock, SOL_SOCKET, SO_REUSEADDR,
		       &yes, sizeof(int)) < 0) {
		logger->log(lvl_warning, "Could not set listener socket "
			    "options: %s.\n", strerror(errno));
	}

	if (listen(m_sock, listen_backlog) < 0) {
		logger->log(lvl_fatal, "Could not listen on port %i: %s\n",
			    m_port, strerror(errno));
	}

	if (pthread_create(&m_listener_thread, NULL,
			   Listener::run_listener_thread, this)) {
		logger->log(lvl_fatal, "Could not spawn listener thread: %s\n",
			    strerror(errno));
	}
}

void *Listener::run_listener_thread(void *parm)
{
	Listener *listener = (Listener *)parm;
	
	while (1) {
		struct sockaddr addr;
		socklen_t sin_size;
		int new_sock = accept(listener->m_sock, &addr, &sin_size);

		if (new_sock < 0) {
			logger->log(lvl_error, "Error while listening on"
				    "port %i: %s\n",
				    listener->get_port_number(),
				    strerror(errno));
		}

		// XXX
		logger->log(lvl_info, "Recieved connection\n"); 
		close(new_sock);
	}
}

Listener::~Listener()
{
	int status = pthread_cancel(m_listener_thread);
	mmtn_assert(!status);

	if (close(m_sock) < 0) {
		logger->log(lvl_error, "Could not close socket on port %i:"
			    " %s\n", m_port, strerror(errno));
	}
}
