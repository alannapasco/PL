package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.VarType;

public class ParentTest {
    final AST one;
    final AST negfive;
    final AST t;
    final AST f;

    AST andTT;
    AST andTF;
    AST andFT;
    AST andFF;

    AST orTT;
    AST orTF;
    AST orFT;
    AST orFF;


    IMeaning boolrepT;
    IMeaning boolrepF;

    Accumulator<VarType> typeAcc;
    Accumulator<IMeaning> valAcc;

    int numLets;
    String[] sdAcc;
    IMeaning[] sdValAcc;

    //examples
    public ParentTest(){
        this.one = new ASTInteger(1);
        this.negfive = new ASTInteger(-5);
        this.t = new ASTBoolean(true);
        this.f = new ASTBoolean(false);

        this.andTT = new ASTAnd(t, t);
        this.andTF = new ASTAnd(t, f);
        this.andFT = new ASTAnd(f, t);
        this.andFF = new ASTAnd(f, f);

        this.orTT = new ASTOr(t, t);
        this.orTF = new ASTOr(t, f);
        this.orFT = new ASTOr(f, t);
        this.orFF = new ASTOr(f, f);

        this.boolrepT = new BooleanRepresentation(true);
        this.boolrepF = new BooleanRepresentation(false);

        this.typeAcc = new Accumulator<>();
        this.valAcc = new Accumulator<>();

        this.numLets = 0;
        this.sdAcc = new String[numLets];
        this.sdValAcc = new IMeaning[numLets];

    }
}
