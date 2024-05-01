package karina.types.path

class MPackage {
    val parent: Option[MPackage] = None
    val childPackages: List[MPackage] = List()
    val childUnits: List[MUnit] = List()
}
