with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
--  with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Hash;
with Ada.Strings.Unbounded;

package body Minerva.Names is

   function To_Name
     (Key_Text   : String;
      Cased_Text : String)
      return Minerva_Name;

   function To_Lower (S : String) return String
                      renames Ada.Characters.Handling.To_Lower;

   function "="
     (Left : Ada.Strings.Unbounded.Unbounded_String;
      Right : String)
      return Boolean
      renames Ada.Strings.Unbounded."=";

   function "+"
     (S : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Name_Entry is
      record
         Key_Text   : Ada.Strings.Unbounded.Unbounded_String;
         Cased_Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Name_Entry_Vectors is
     new Ada.Containers.Vectors
       (Minerva_Name_Index, Name_Entry);

   package Name_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Minerva_Name_Index,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   Name_Vector : Name_Entry_Vectors.Vector;
   Name_Map    : Name_Maps.Map;

   ---------
   -- "=" --
   ---------

   function "="
     (Left  : Minerva_Name;
      Right : String)
      return Boolean
   is
      Name : Name_Entry renames Name_Vector (Left.Index);
   begin
      return Name.Key_Text = Right;
   end "=";

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Name : Minerva_Name) return Minerva_Name is
      Parts : constant Minerva_Name_Array := Split (Name);
   begin
      return Parts (Parts'Last);
   end Base_Name;

   ----------------
   -- Cased_Text --
   ----------------

   function Cased_Text
     (Name : Minerva_Name)
      return String
   is
      Result : constant String :=
                 Ada.Strings.Unbounded.To_String
                   (Name_Vector.Element (Name.Index).Cased_Text);
   begin
      return Result;
   end Cased_Text;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Name    : Minerva_Name;
      Process : not null access
        procedure (Parent : Minerva_Name;
                   Child  : Minerva_Name;
                   Last   : Boolean))

   is
      Parts : constant Minerva_Name_Array := Split (Name);
      Parent : Minerva_Name;
   begin
      for I in Parts'Range loop
         Process (Parent, Parts (I), I = Parts'Last);
         Parent := Join (Parent, Parts (I));
      end loop;
   end Iterate;

   ----------
   -- Join --
   ----------

   function Join (Names : Minerva_Name_Array) return Minerva_Name is
      use Ada.Strings.Unbounded;
      Standard_Result : Unbounded_String;
      Cased_Result    : Unbounded_String;
   begin

      for Name of Names loop
         if Name.Index /= 0 then
            declare
               Element : Name_Entry renames Name_Vector (Name.Index);
            begin
               if Standard_Result /= Null_Unbounded_String then
                  Standard_Result := Standard_Result & ".";
                  Cased_Result    := Cased_Result & ".";
               end if;
               Standard_Result := Standard_Result & Element.Key_Text;
               Cased_Result := Cased_Result & Element.Cased_Text;
            end;
         end if;
      end loop;

      declare
         Standard_Text : constant String := To_String (Standard_Result);
      begin
         if Name_Map.Contains (Standard_Text) then
            return (Index => Name_Map.Element (Standard_Text));
         else
            Name_Vector.Append (Name_Entry'
                                  (Key_Text   => Standard_Result,
                                   Cased_Text => Cased_Result));
            Name_Map.Insert (Standard_Text, Name_Vector.Last_Index);
            return (Index => Name_Vector.Last_Index);
         end if;
      end;
   end Join;

   ----------
   -- Join --
   ----------

   function Join (Left, Right : Minerva_Name) return Minerva_Name is
      Arr : constant Minerva_Name_Array := [Left, Right];
   begin
      return Join (Arr);
   end Join;

   ----------------
   -- Qualifiers --
   ----------------

   function Qualifiers (Name : Minerva_Name) return Minerva_Name_Array is
      Parts : constant Minerva_Name_Array := Split (Name);
   begin
      return Parts (Parts'First .. Parts'Last - 1);
   end Qualifiers;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Name    : Minerva_Name;
      Process : not null access
        procedure (Partial_Name : Minerva_Name))
   is
      procedure Process_Item
        (Parent : Minerva_Name;
         Child  : Minerva_Name;
         Last   : Boolean);

      ------------------
      -- Process_Item --
      ------------------

      procedure Process_Item
        (Parent : Minerva_Name;
         Child  : Minerva_Name;
         Last   : Boolean)
      is
         pragma Unreferenced (Last);
      begin
         Process (Join (Parent, Child));
      end Process_Item;

   begin
      Iterate (Name, Process_Item'Access);
   end Scan;

   -----------
   -- Split --
   -----------

   function Split (Name : Minerva_Name) return Minerva_Name_Array is
      Key_Text : constant String := Standard_Text (Name);
      Cased_Text : constant String := Minerva.Names.Cased_Text (Name);

      Array_Start : Positive := Key_Text'First;
      Array_Last : Natural := 0;

      Result     : Minerva_Name_Array (1 .. 20);

      --  function Scan (Start : Positive) return Minerva_Name_Array;

      ----------
      -- Scan --
      ----------

      --  function Scan (Start : Positive) return Minerva_Name_Array is
      --     Next : constant Natural :=
      --              Ada.Strings.Fixed.Index
      --                (Source  => Key_Text,
      --                 Pattern => ".",
      --                 From    => Start,
      --                 Going   => Ada.Strings.Forward);
      --     Last : constant Natural :=
      --              (if Next = 0
      --               then Key_Text'Last
      --               else Next - 1);
      --     Key  : constant String := Key_Text (Start .. Last);
      --     Cased : constant String := Cased_Text (Start .. Last);
      --
      --     Head : constant Minerva_Name :=
      --              To_Name (Key, Cased);
      --  begin
      --     if Next = 0 then
      --        return (1 => Head);
      --     else
      --        return Head & Scan (Next + 1);
      --     end if;
      --  end Scan;

   begin
      if Key_Text = "" then
         return Result : Minerva_Name_Array (1 .. 0);
      end if;

      loop
         declare
            Next : constant Natural :=
                     Ada.Strings.Fixed.Index (Key_Text, ".", Array_Start);
            Last  : constant Natural :=
                      (if Next = 0
                       then Key_Text'Last
                       else Next - 1);
            Key   : constant String := Key_Text (Array_Start .. Last);
            Cased : constant String := Cased_Text (Array_Start .. Last);

            Head  : constant Minerva_Name :=
                      To_Name (Key, Cased);
         begin
            Array_Last := Array_Last + 1;
            Result (Array_Last) := Head;
            exit when Next = 0;
            Array_Start := Next + 1;
         end;
      end loop;
      return Result (1 .. Array_Last);

      --  return Scan (Key_Text'First);
   end Split;

   -------------------
   -- Standard_Text --
   -------------------

   function Standard_Text
     (Name : Minerva_Name)
      return String
   is
      Result : constant String :=
                 Ada.Strings.Unbounded.To_String
                   (Name_Vector.Element (Name.Index).Key_Text);
   begin
      return Result;
   end Standard_Text;

   -------------
   -- To_Name --
   -------------

   function To_Name
     (Key_Text   : String;
      Cased_Text : String)
      return Minerva_Name
   is
   begin
      if Key_Text = "" then
         return (Index => 0);
      else
         declare
            use Name_Maps;
            Position   : constant Cursor :=
                           Name_Map.Find (Key_Text);
         begin
            if Has_Element (Position) then
               return (Index => Element (Position));
            else
               Name_Vector.Append
                 (Name_Entry'
                    (Key_Text     => +Key_Text,
                     Cased_Text   => +Cased_Text));
               Name_Map.Insert (Key_Text, Name_Vector.Last_Index);
               return (Index => Name_Vector.Last_Index);
            end if;
         end;
      end if;
   end To_Name;

   -------------
   -- To_Name --
   -------------

   function To_Name
     (Id             : String;
      Case_Sensitive : Boolean := False)
      return Minerva_Name
   is
      Key : constant String :=
              (if Case_Sensitive
               then Id
               else To_Lower (Id));
   begin
      return To_Name (Key, Id);
   end To_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Name       : Minerva_Name;
      Separator  : String;
      Lower_Case : Boolean)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Part of Split (Name) loop
         if Result /= "" then
            Result := Result & Separator;
         end if;
         Result := Result &
         (if Lower_Case
          then To_Lower (Cased_Text (Part))
          else Cased_Text (Part));
      end loop;
      return To_String (Result);
   end To_String;

begin
   Name_Map.Insert ("", 0);
   Name_Vector.Append (Name_Entry'
                         (Key_Text   => <>,
                          Cased_Text => <>));
end Minerva.Names;
