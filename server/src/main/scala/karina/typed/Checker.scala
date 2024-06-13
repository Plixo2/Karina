package karina.typed

import karina.LanguageException
import karina.typed.InferTransformer.TypeCheckContext
import karina.types.*

import scala.util.boundary

object Checker {

    /**
     * Check if a is assignable to b
     * So that a = b is valid
     * @param a must be a subtype of b
     * @param b must be a supertype of a
     * @param mutate if true, generics will be mutated
     * @return
     */
    def isAssignable(context: TypeCheckContext, a: Type, b: Type, mutate: Boolean): Boolean = {
        a match {
            case resolvable: Resolvable => {
                return if (resolvable.isResolved) {
                    isAssignable(context, resolvable.get(), b, mutate)
                } else {
                    if (mutate && resolvable != b) {
                        // todo check cycles
                        resolvable.resolve(b)
                    }
                    true
                }
            }
            case _ => {}
        }
        b match {
            case resolvable: Resolvable => {
                return if (resolvable.isResolved) {
                    isAssignable(context, a, resolvable.get(), mutate)
                } else {
                    if (mutate && resolvable != a) {
                        // todo check cycles
                        resolvable.resolve(a)
                    }
                    true
                }
            }
            case _ => {}
        }

        a match {
            case ObjectTypeDefault(region, path, generics) =>
                throw new LanguageException(region, "Cannot test default object type")
            case GenericType(region, name, source) => {
                b match {
                    case GenericType(region, name2, source2) => {
                        name.equals(name2) && source.equals(source2)
                    }
                    case _ => false
                }
            }
            case FunctionType(region, args, ret) => {
                b match {
                    case FunctionType(region, args2, ret2) => {
                        if (args.length != args2.length) {
                            return false
                        }
                        val argsCompatible = {
                            boundary {
                                for (i <- args.indices) {
                                    if (!isAssignable(context, args2(i), args(i), mutate)) {
                                        boundary.break(false)
                                    }
                                }
                                true
                            }
                        }
                        if (!argsCompatible) {
                            return false
                        }
                        println(s"Checking return type ${ret} = ${ret2}")
                        isAssignable(context, ret, ret2, mutate)
                    }
                    case _ => false
                }

            }
            case ArrayType(region, t) => {
                b match {
                    case ArrayType(region, t2) => isAssignable(context, t, t2, mutate)
                    case _                     => false
                }
            }
            case FloatType(region) => {
                b match {
                    case FloatType(region) => true
                    case _                 => false
                }
            }
            case VoidType(region) => {
                b match {
                    case VoidType(region) => true
                    case _                => false
                }
            }
            case IntegerType(region) => {
                b match {
                    case IntegerType(region) => true
                    case _                   => false
                }
            }
            case StringType(region) =>
                b match {
                    case StringType(region) => true
                    case _                  => false
                }
            case BooleanType(region) =>
                b match {
                    case BooleanType(region) => true
                    case _                   => false
                }
            case resolvable: Resolvable => {
                throw new LanguageException(resolvable.getRegion(), "should not reach here")
            }
            case ObjectType(region, path, binds) => {
                val objectClass = context.root.locateClass(path).expectWithRegion(region, "Class A not found")
                b match {
                    case ObjectType(region, subPath, subBinds) => {
                        if (path.equals(subPath)) {
                            if (binds.size != subBinds.size) {
                                return false
                            }
                            val genericsBounds = binds.map((k, left) => {
                                val right = subBinds(k)
                                (right, left)
                            })
                            genericsBounds.foldLeft(true)((acc, tuple) => {
                                if (!acc) {
                                    false
                                } else {
                                    isAssignable(context, tuple._1, tuple._2, mutate)
                                }
                            })
                        } else {
                            false
                        }
                    }
                    case _ => false
                }
            }
            case BaseType(region) => {
                true
            }
        }
    }
}
