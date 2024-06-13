package karina.codegen.jvm

import com.sun.org.apache.bcel.internal.generic.IRETURN
import karina.typed.DefaultFunction
import org.objectweb.asm.tree.MethodNode

import java.lang.reflect.Modifier

object CodeGen {
    
    

    def generateFunction(function: DefaultFunction): MethodNode = {
        val methodNode = new MethodNode()
        methodNode.access = Modifier.PUBLIC
        if (function.isStatic) {
            methodNode.access |= Modifier.STATIC
        }
        methodNode.name = function.functionName
        methodNode.desc = ???
        

        methodNode
    }

    

}
