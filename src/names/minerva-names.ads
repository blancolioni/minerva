private package Minerva.Names is

   type Minerva_Name is private;

   function To_Name
     (Id             : String;
      Case_Sensitive : Boolean := False)
      return Minerva_Name;

   function "="
     (Left  : Minerva_Name;
      Right : String)
      return Boolean;

   function Is_Empty
     (Name : Minerva_Name)
      return Boolean;

   function Standard_Text
     (Name : Minerva_Name)
      return String;

   function Cased_Text
     (Name : Minerva_Name)
      return String;

   function To_String
     (Name       : Minerva_Name;
      Separator  : String;
      Lower_Case : Boolean)
      return String;

   type Minerva_Name_Array is array (Positive range <>) of Minerva_Name;

   function Join (Left, Right : Minerva_Name) return Minerva_Name;
   function Join (Names : Minerva_Name_Array) return Minerva_Name;
   function Split (Name : Minerva_Name) return Minerva_Name_Array;

   function Qualifiers (Name : Minerva_Name) return Minerva_Name;
   function Base_Name (Name : Minerva_Name) return Minerva_Name;

   procedure Iterate
     (Name : Minerva_Name;
      Process : not null access
        procedure (Parent : Minerva_Name;
                   Child  : Minerva_Name;
                   Last   : Boolean));

   procedure Scan
     (Name    : Minerva_Name;
      Process : not null access
        procedure (Partial_Name : Minerva_Name));
   --  Given a name x.y.z, call Process with x, x.y, and x.y.z

   type Name_Interface is interface;

   function Name (Item : Name_Interface) return Minerva_Name is abstract;

   function Has_Name
     (Name : Name_Interface'Class)
      return Boolean;

   function Standard_Text
     (Name : Name_Interface'Class)
      return String;

   function Cased_Text
     (Name : Name_Interface'Class)
      return String;

private

   type Minerva_Name_Index is new Natural;

   type Minerva_Name is
      record
         Index : Minerva_Name_Index := 0;
      end record;

   function Is_Empty
     (Name : Minerva_Name)
      return Boolean
   is (Name.Index = 0);

   function Has_Name
     (Name : Name_Interface'Class)
      return Boolean
   is (not Is_Empty (Name.Name));

   function Standard_Text
     (Name : Name_Interface'Class)
      return String
   is (Standard_Text (Name.Name));

   function Cased_Text
     (Name : Name_Interface'Class)
      return String
   is (Cased_Text (Name.Name));

end Minerva.Names;
