package karina.types

import karina.types.path.MUnit

class MObject {
    val name: String = ""
    val unit: MUnit = null;

    val fields: List[MField] = List()

}

class MField {
    val name: String = ""
    val fieldType: Type = null;
}
