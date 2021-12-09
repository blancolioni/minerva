with Minerva.Ids;
with Minerva.Names;

limited with Minerva.Entries;

private package Minerva.Environment is

   subtype Environment_Id is Minerva.Ids.Environment_Id;

   function Environment_Name
     (Environment : Minerva.Ids.Environment_Id)
      return String;

   function Exists
     (Environment : Minerva.Ids.Environment_Id;
      Name        : String)
      return Boolean;

   function Exists
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name)
      return Boolean;

   function Exists_Locally
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name)
      return Boolean;

   function Get
     (Environment : Minerva.Ids.Environment_Id;
      Name        : String)
      return Minerva.Entries.Constant_Class_Reference
     with Pre => Exists (Environment, Name);

   function Get
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name)
      return Minerva.Entries.Constant_Class_Reference
     with Pre => Exists (Environment, Name);

   procedure Iterate_Matches
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name;
      Matches     : not null access
        function (Test_Entry : Minerva.Entries.Constant_Class_Reference)
      return Boolean;
      Process     : not null access
        procedure (Found_Entry : Minerva.Entries.Constant_Class_Reference))
     with Pre => Minerva.Names.Qualifiers (Name)'Length = 0;

   procedure Iterate_Names
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name;
      Process     : not null access
        procedure (Found_Entry : Minerva.Entries.Constant_Class_Reference))
     with Pre => Minerva.Names.Qualifiers (Name)'Length = 0;

   procedure Insert
     (Environment : Minerva.Ids.Environment_Id;
      Element     : not null access constant Minerva.Entries.Instance'Class);

   procedure Use_Environment
     (Environment      : Environment_Id;
      Used_Environment : Environment_Id);

   function Current_Frame_Offset
     (Environment : Minerva.Ids.Environment_Id)
      return Natural;

   procedure Set_Frame_Offset
     (Environment : Minerva.Ids.Environment_Id;
      New_Offset  : Natural);

   function Parent
     (Environment : Minerva.Ids.Environment_Id)
      return Minerva.Ids.Environment_Id_Base;

   function Create_Standard_Environment
     return Minerva.Ids.Environment_Id;

   function Create_Environment
     (Name   : Minerva.Names.Minerva_Name;
      Parent : Minerva.Ids.Environment_Id)
      return Minerva.Ids.Environment_Id;

   function Create_Internal_Environment
     (Name   : Minerva.Names.Minerva_Name)
      return Minerva.Ids.Environment_Id;

   procedure Add_Parent
     (This   : Environment_Id;
      Parent : Environment_Id);

end Minerva.Environment;
