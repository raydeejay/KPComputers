package kpc.common.computer.api;

import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoSolutionException;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Theory;
import kpc.api.fs.FileSystem;
import kpc.api.fs.io.InputStream;

import java.io.IOException;

public final class PrologApi{
    private final FileSystem fs;
    private final Prolog prolog = new Prolog();

    public PrologApi(FileSystem fs){
        this.fs = fs;
    }

    public Object solve(String str){
        try {
            SolveInfo info = this.prolog.solve(str);
            if(info.isSuccess()){
                StringBuilder builder = new StringBuilder("[");
                for(int i = 0; i < info.getBindingVars().size(); i++){
                    builder.append(info.getBindingVars().get(i));
                    if(i < info.getBindingVars().size() - 1){
                        builder.append(",");
                    }
                }
                return builder.append("]").toString();
            }

            return "no.";
        } catch (MalformedGoalException | NoSolutionException e) {
            e.printStackTrace(System.err);
            return "Exception: " + e.getMessage();
        }
    }

    public Object loadTheory(String path){
        try(InputStream stream = this.fs.openInputStream(path)){
            if(stream == null){
                return "Theory " + path + " not found";
            }

            this.prolog.setTheory(new Theory(stream.toString()));
            return true;
        } catch (IOException | InvalidTheoryException e) {
            return "Exception: " + e.getMessage();
        }
    }
}