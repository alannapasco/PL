package pl.AST;

import org.junit.Test;

import static org.junit.Assert.*;

public class ASTOrTest extends ParentTest{

    @Test
    public void typeCheck() throws Exception {
        assertEquals(VarType.BOOLEAN, orTT.typeCheck(typeAcc));
        assertEquals(VarType.BOOLEAN, orTF.typeCheck(typeAcc));
        assertEquals(VarType.BOOLEAN, orFT.typeCheck(typeAcc));
        assertEquals(VarType.BOOLEAN, orFF.typeCheck(typeAcc));
    }

    @Test(expected = Exception.class)
    public void typeCheckException() throws Exception {
        AST invalid = new ASTOr(t,one);
        invalid.typeCheck(typeAcc);
    }

    @Test
    public void value() throws Exception {
        assertEquals(boolrepT, orTT.value(valAcc));
        assertEquals(boolrepT, orTF.value(valAcc));
        assertEquals(boolrepT, orFT.value(valAcc));
        assertEquals(boolrepF, orFF.value(valAcc));
    }

    @Test(expected = Exception.class)
    public void valueException() throws Exception {
        AST invalid = new ASTOr(t,one);
        invalid.value(valAcc);
    }

    @Test
    public void staticDistance() {
        assertEquals(orTF, orTF.staticDistance(sdAcc, numLets));
        assertEquals(orFT, orFT.staticDistance(sdAcc, numLets));
    }

    @Test
    public void countNumLets() {
        assertEquals(0, orFT.countNumLetsInAST(0));
    }

    @Test
    public void valueSD() throws Exception {
        assertEquals(boolrepT, orTT.valueSD(sdValAcc, numLets));
        assertEquals(boolrepF, orFF.valueSD(sdValAcc, numLets));
    }

    @Test(expected = Exception.class)
    public void valueSDException() throws Exception {
        AST invalid = new ASTOr(t,one);
        invalid.valueSD(sdValAcc, numLets);
    }
}