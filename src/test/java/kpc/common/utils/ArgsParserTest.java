package kpc.common.utils;


import java.util.Arrays;

public class ArgsParserTest {
    public void testParse()
    throws Exception {
        System.out.println(Arrays.toString(ArgsParser.parse("\"Hello World\" \"Test World\"")));
    }
}