package me.botsko.prism;

import junit.framework.TestCase;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 2/09/2020.
 */
public class Il8nTest extends TestCase {


    public void testGetMessage() {
    }

    public void testGetRawMessage() {
        assertEquals("Prism - originally by Viveleroi; maintained by <author> v<version>", Il8n.getRawMessage("about-header"));
    }

    public void testFormatMessage() {
    }
}