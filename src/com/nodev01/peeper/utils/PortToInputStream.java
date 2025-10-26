package com.nodev01.peeper.utils;
import gnu.mapping.InPort;

import java.io.IOException;
import java.io.InputStream;

public class PortToInputStream extends InputStream {

	private InPort inport;
	
	public PortToInputStream(InPort inport) {
		this.inport = inport;
	}
	
	
	public int available() throws IOException {
		return 1; 
	}


	public int read() throws IOException {
		return inport.read();
	}

}
