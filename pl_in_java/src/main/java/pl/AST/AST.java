package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

/**
 AST is a tree-shaped diagram
 AST is one of:
 -bool
 -int
 -[AST "^" AST]
 -[AST "||" AST]
 -[AST ">" AST]
 -[AST "-" AST]
 -[AST "+" AST]
 -Error Node

 AST interface has 2 methods (so far):
    -> typeCheck - ret TypePrediction
    -> value - ret IMeaning (will have to down cast)
 (parse is a static function in Main that returns an AST)

 TypePrediction is one of: (could be an enum)
 - TypeBoolean -- "this will be a boolean"
 - TypeInteger -- "this will be an integer"

 IMeaning is one of:
 - a boolean
 - an integer
 */

public interface AST {

    /**
     * Classifies the expression as either int type or bool type
     */
    Type typeCheck(Accumulator<Type> accumulator) throws Exception;

    /**
     * Determines the value of the AST expression
     */
    IMeaning value(Accumulator<IMeaning> accumulator) throws Exception;

    /**
     * Returns a brand new AST with all ASTName nodes turned to ASTStaticDistance nodes
     * Integer in Accumulator<Integer> represents the current node's depth in the AST
     */
    AST staticDistance(Accumulator<Integer> accumulator);

    /**
     * Returns the number of let expressions in an AST
     */
    int countNumLetsInAST(int count);

    /**
     * Interprets the value of an AST that contains SD nodes
     */
    IMeaning valueSD(IMeaning[] acc, int nextFreeSlot) throws Exception;
}
