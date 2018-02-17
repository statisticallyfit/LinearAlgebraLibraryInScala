package linalg.syntax

//import spire.syntax.std.DoubleSyntax

/**
  *
  */
//note: no need to add IntSyntax etc here because the implicit classes in these below names
//note - account for taking int as their types - check TODO

trait AllSyntax
     extends AbsoluteSyntax
     with RootSyntax
     with FieldSyntax
     with NumberSyntax
     with TrigSyntax
     with EqualitySyntax
     //with VectorSpaceSyntax
     with DimensionSyntax
     with ShowSyntax


