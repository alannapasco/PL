package pl.AST;

import org.junit.Test;

import static org.junit.Assert.*;

public class ASTAndTest extends ParentTest {

    @Test
    public void typeCheck() throws Exception {
        assertEquals(VarType.BOOLEAN, andTT.typeCheck(typeAcc));
        assertEquals(VarType.BOOLEAN, andTF.typeCheck(typeAcc));
        assertEquals(VarType.BOOLEAN, andFT.typeCheck(typeAcc));
        assertEquals(VarType.BOOLEAN, andFF.typeCheck(typeAcc));
    }

    @Test(expected = Exception.class)
    public void typeCheckException() throws Exception {
        AST invalid = new ASTAnd(t,one);
        invalid.typeCheck(typeAcc);
    }


    @Test
    public void value() throws Exception {
        assertEquals(boolrepT, andTT.value(valAcc));
        assertEquals(boolrepF, andTF.value(valAcc));
        assertEquals(boolrepF, andFT.value(valAcc));
        assertEquals(boolrepF, andFF.value(valAcc));
    }

    @Test(expected = Exception.class)
    public void valueException() throws Exception {
        AST invalid = new ASTAnd(t,one);
        invalid.value(valAcc);
    }

    @Test
    public void staticDistance() {
        assertEquals(andTT, andTT.staticDistance(sdAcc, numLets));
        assertEquals(andFF, andFF.staticDistance(sdAcc, numLets));
    }

    @Test
    public void countNumLets() {
        assertEquals(0, andTT.countNumLetsInAST(0));
    }

    @Test
    public void valueSD() throws Exception {
        assertEquals(boolrepT, andTT.valueSD(sdValAcc, numLets));
        assertEquals(boolrepF, andTF.valueSD(sdValAcc, numLets));
        assertEquals(boolrepF, andFT.valueSD(sdValAcc, numLets));
        assertEquals(boolrepF, andFF.valueSD(sdValAcc, numLets));
    }

    @Test(expected = Exception.class)
    public void valueSDException() throws Exception {
        AST invalid = new ASTAnd(t,one);
        invalid.valueSD(sdValAcc, numLets);
    }
}