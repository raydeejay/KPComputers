import kpc.api.fs.FileSystem;
import kpc.common.computer.fs.Ext9001FileSystem;
import org.apache.commons.io.IOUtils;

import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;

public final class TestFileSystem{
    public void test()
    throws Exception{
        Path p = Paths.get(System.getProperty("user.home"), "Desktop", "ext9001");
        FileSystem fs = new Ext9001FileSystem();
        LinkedList<String> files = new LinkedList<>();

        fs.list("", files);
        for(String str : files){
            System.out.println(str);
        }

        fs.mkdir("test");

        files.clear();
        fs.list("", files);
        for(String str : files){
            System.out.println(str);
        }

        try(OutputStream os = fs.openOutputStream("test.scm")){
            os.write("(println \"Hello World\")".getBytes(StandardCharsets.UTF_8));
            os.flush();
        }

        System.out.println(IOUtils.toString(fs.openInputStream("test.scm")));
    }
}