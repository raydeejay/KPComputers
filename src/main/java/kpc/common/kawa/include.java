package kpc.common.kawa;

import gnu.expr.Expression;
import gnu.expr.ScopeExp;
import gnu.kawa.io.BinaryInPort;
import gnu.kawa.io.InPort;
import gnu.kawa.io.Path;
import gnu.kawa.lispexpr.LispReader;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.lists.Sequence;
import gnu.text.Lexer;
import kawa.lang.Syntax;
import kawa.lang.Translator;
import kpc.api.fs.FileSystem;

import java.io.File;
import java.io.InputStream;
import java.nio.charset.Charset;

public final class include
extends Syntax {
    private final FileSystem fs;

    public include(FileSystem fs){
        super("include");
        this.fs = fs;
    }

    @Override
    public void scanForm(Pair st, ScopeExp defs, Translator tr){
        this.process(st.getCdr(), tr, defs);
    }

    @Override
    public Expression rewrite(Object obj, Translator tr){
        return tr.rewrite(this.process(obj, tr, null));
    }

    private Object process(Object rest, Translator tr, ScopeExp defs){
        LList result = LList.Empty;

        Pair pair;
        for(Pair lastPair = null; rest instanceof Pair; rest = pair.getCdr()){
            pair = (Pair) rest;

            Object pairCar = pair.getCar();
            Object savePos = tr.pushPositionOf(pair);

            if(!(pairCar instanceof CharSequence)){
                tr.error('e', "find parameters must be strings");
            }

            String fName = pairCar.toString();

            Path path;
            BinaryInPort inp;
            while(true){
                File f = this.fs.resolve("/include/" + fName).toFile();
                System.out.println(f);
                Path reader = Path.valueOf(f);

                try{
                    path = reader.resolve(fName);
                    InputStream lexer = path.openInputStream();

                    try{
                        inp = BinaryInPort.openHeuristicFile(lexer, path);
                        break;
                    } catch(Exception e){
                        tr.error('e', "error reading file \"" + path + "\": " + e.getMessage());
                        return result;
                    }
                } catch(Exception e){
                    // Fallthrough
                }
            }

            tr.popPositionOf(savePos);
            LispReader reader = new LispReader(inp, tr.getMessages());
            Lexer lexer = tr.lexer;
            tr.lexer = reader;

            try{
                if(inp.getCharset() == null && lexer != null){
                    InPort sexp = lexer.getPort();
                    if(sexp instanceof BinaryInPort){
                        Charset npair = ((BinaryInPort) sexp).getCharset();
                        if(npair != null){
                            inp.setDefaultCharset(npair);
                        }
                    }
                }

                while(true){
                    Object sexp;
                    try{
                        sexp = reader.readCommand();
                        if(sexp == Sequence.eofValue){
                            break;
                        }
                    } catch(Exception e){
                        tr.error('e', "error reading file \"" + path + "\": " + e.getMessage());
                        return result;
                    }

                    if(defs != null){
                        tr.scanForm(sexp, defs);
                    } else{
                        Pair npair = new Pair(sexp, LList.Empty);
                        if(lastPair == null){
                            result = npair;
                        } else{
                            lastPair.setCdrBackdoor(npair);
                        }

                        lastPair = npair;
                    }
                }
            } finally{
                tr.lexer = lexer;
            }
        }

        if(rest != LList.Empty){
            tr.error('e', "improper list");
        }

        return result;
    }
}