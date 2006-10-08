/**
 * Monster Mountain client
 * Copyright (C) 2006 Nick Thomas
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation; the terms of
 * any later version are NOT APPLICABLE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

package com.nwthomas.mmtn.client;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.io.*;

public class Client extends JApplet {

	private JTextArea displayWindow;
	private JScrollPane scrollPane;
	private JTextField input;
	private Socket sock;
	private volatile boolean socketListenerGoAwayPlzKthx;
	private static final long serialVersionUID = 1L;
	
	public void init() {
		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					createGUI();
				}
			});
		} catch (Exception e) {
			System.err.println("No soup for you!");
		}
	}

	private void createGUI() {
		Font courier = new Font("Courier", Font.PLAIN, 12);
		
		displayWindow = new JTextArea();
		displayWindow.setBackground(Color.black);
		displayWindow.setForeground(Color.white);
		displayWindow.setEditable(false);
		displayWindow.setFont(courier);
		displayWindow.setColumns(80);
		displayWindow.setLineWrap(true);
		displayWindow.setWrapStyleWord(true);

		scrollPane = new JScrollPane(displayWindow, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		
		input = new JTextField();
		input.setBackground(Color.black);
		input.setForeground(Color.white);
		input.setFont(courier);
		input.setColumns(80);
		Dimension dim = input.getMinimumSize();
		dim.width = input.getMaximumSize().width;
		input.setMaximumSize(dim);
		input.getInputMap().put(KeyStroke.getKeyStroke("ENTER"), "run");
		input.getActionMap().put("run", new AbstractAction() {
			private static final long serialVersionUID = 1L;

			public void actionPerformed(ActionEvent e) {
				displayWindow.append(input.getText());
				displayWindow.append("\n");
				input.setText("");
			}
		});

		java.awt.Container contentPane = getContentPane();
		contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
		contentPane.add(scrollPane);
		contentPane.add(input);
	}
	
	public void start() {
		displayWindow.append("Connecting to server...\n");
		
		String host = getCodeBase().getHost();
		
		try {
			InetAddress addr = InetAddress.getByName(host);
			sock = new Socket(addr, 4141);
			
			new Thread(new Runnable() {
				public void run() {
					try {
						InputStream stream = sock.getInputStream();
						Reader reader = new InputStreamReader(stream, "US-ASCII");
						char[] buf = new char[512];
						
						while (!socketListenerGoAwayPlzKthx) {
							int n = reader.read(buf);
							String s = new String(buf, 0, n);
							displayWindow.append(s);
						}
					} catch (IOException e) {
						displayWindow.append("I/O exception on socket\n");
					}
				}
			});
			
			displayWindow.append("Connected!\n");
		} catch (UnknownHostException e) {
			displayWindow.append("Could not resolve host " + host + ".\n");
		} catch (IOException e) {
			displayWindow.append("Could not connect to host " + host + ": " 
					+ e.getMessage() + ".\n");
		}
	}
	
	public void stop() {
		socketListenerGoAwayPlzKthx = true;
	}
}
