package body Minerva.Trees.Types is

   -----------------------
   -- Set_Defining_Name --
   -----------------------

   procedure Set_Defining_Name
     (This : in out Class;
      Name : Minerva.Names.Minerva_Name)
   is
   begin
      This.Defining_Name := Name;
   end Set_Defining_Name;

end Minerva.Trees.Types;
