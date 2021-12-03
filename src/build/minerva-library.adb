with Ada.Characters.Handling;

with WL.String_Maps;

with Minerva.Files;
with Minerva.Parser;

with Minerva.Logging;

package body Minerva.Library is

   package Tree_Maps is
     new WL.String_Maps (Minerva.Trees.Declarations.Class_Reference,
                         Minerva.Trees.Declarations."=");

   Spec_Map : Tree_Maps.Map;
   Body_Map : Tree_Maps.Map;

   function To_File_Name
     (Subprogram_Name : String;
      Specification   : Boolean)
     return String;

   function Load_Subprogram
     (Subprogram_Name : Minerva.Names.Minerva_Name;
      Specification   : Boolean)
      return Minerva.Trees.Declarations.Class_Reference;

   --------------------
   -- Iterate_Bodies --
   --------------------

   procedure Iterate_Bodies
     (Process : not null access
        procedure (Subprogram_Body : Trees.Declarations.Class_Reference))
   is
   begin
      for Item of Body_Map loop
         Process (Item);
      end loop;
   end Iterate_Bodies;

   ---------------
   -- Load_Body --
   ---------------

   function Load_Body
     (Subprogram_Name : Minerva.Names.Minerva_Name)
      return Minerva.Trees.Declarations.Class_Reference
   is
   begin
      return Load_Subprogram (Subprogram_Name,
                              Specification => False);
   end Load_Body;

   ------------------------
   -- Load_Specification --
   ------------------------

   function Load_Specification
     (Subprogram_Name : Minerva.Names.Minerva_Name)
      return Minerva.Trees.Declarations.Class_Reference
   is
   begin
      return Load_Subprogram (Subprogram_Name, Specification => True);
   end Load_Specification;

   ---------------------
   -- Load_Subprogram --
   ---------------------

   function Load_Subprogram
     (Subprogram_Name : Minerva.Names.Minerva_Name;
      Specification   : Boolean)
      return Minerva.Trees.Declarations.Class_Reference
   is
      use Minerva.Trees.Declarations;
      Tree      : Class_Reference;

      Parent    : Minerva.Names.Minerva_Name;

      procedure Load
        (Name : Minerva.Names.Minerva_Name;
         Last : Boolean);

      ----------
      -- Load --
      ----------

      procedure Load
        (Name : Minerva.Names.Minerva_Name;
         Last : Boolean)
      is
         Full_Name : constant String :=
                       Minerva.Names.Standard_Text
                         (Minerva.Names.Join (Parent, Name));
         Load_Spec : constant Boolean :=
                       not Last or else Specification;
         Cached    : Boolean := False;
      begin
         if Load_Spec then
            if Spec_Map.Contains (Full_Name) then
               Tree := Spec_Map (Full_Name);
               Cached := True;
            end if;
         else
            if Body_Map.Contains (Full_Name) then
               Tree := Body_Map (Full_Name);
               Cached := True;
            end if;
         end if;

         if not Cached then
            declare
               File_Name : constant String :=
                             To_File_Name
                               (Full_Name, Load_Spec);
               File_Path : constant String :=
                             Minerva.Files.Find_File (File_Name);
            begin
               if File_Path = "" then
                  Minerva.Logging.Log ("cannot find: " & File_Name);
                  Tree := null;
               else
                  Minerva.Logging.Log ("loading: " & File_Path);
                  Tree :=
                    Class_Reference
                      (Minerva.Parser.Parse_File (File_Path));
                  if Load_Spec then
                     Spec_Map.Insert (Full_Name, Tree);
                  else
                     Body_Map.Insert (Full_Name, Tree);
                  end if;
               end if;
            end;
         end if;

         Parent := Minerva.Names.Join (Parent, Name);

      end Load;

      Names : constant Minerva.Names.Minerva_Name_Array :=
                Minerva.Names.Split (Subprogram_Name);
   begin
      Minerva.Logging.Log ("library: loading "
                           & Minerva.Names.Cased_Text (Subprogram_Name));

      for I in Names'Range loop
         Load (Names (I), I = Names'Last);
      end loop;
      return Tree;
   end Load_Subprogram;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name
     (Subprogram_Name : String;
      Specification   : Boolean)
      return String
   is
      use Ada.Characters.Handling;
      File_Name : String := Subprogram_Name;
   begin
      for Ch of File_Name loop
         if Is_Letter (Ch) then
            Ch := To_Lower (Ch);
         elsif Ch = '.' then
            Ch := '-';
         end if;
      end loop;
      return File_Name
        & (if Specification then ".ads" else ".adb");
   end To_File_Name;

end Minerva.Library;
