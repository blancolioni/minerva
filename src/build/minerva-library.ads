with Minerva.Names;
with Minerva.Trees.Declarations;

private package Minerva.Library is

   function Load_Specification
     (Subprogram_Name : Minerva.Names.Minerva_Name)
      return Minerva.Trees.Declarations.Class_Reference;

   function Load_Body
     (Subprogram_Name : Minerva.Names.Minerva_Name)
      return Minerva.Trees.Declarations.Class_Reference;

   procedure Iterate_Bodies
     (Process : not null access
        procedure (Subprogram_Body : Trees.Declarations.Class_Reference));

end Minerva.Library;
