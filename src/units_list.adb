----------------------------------------------------------------------
--  Units_List - Package body                                       --
--  Copyright (C) 2002 Adalog                                       --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                   Tel: +33 1 41 24 31 40                 --
--  19-21 rue du 8 mai 1945  Fax: +33 1 41 24 07 36                 --
--  94110 ARCUEIL            E-m: info@adalog.fr                    --
--  FRANCE                   URL: http://www.adalog.fr              --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
----------------------------------------------------------------------

with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Fixed,
  Ada.Unchecked_Deallocation,
  Ada.Wide_Text_IO;

with   -- ASIS components
  Asis.Compilation_Units,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;
pragma Elaborate_All (Asis.Iterator);

with   -- Reusable components
  A4G_Bugs,
  Thick_Queries,
  Utilities;
package body Units_List is
   use Utilities;

   ----------------------------------------------------------------
   --                 Internal elements                          --
   ----------------------------------------------------------------

   ------------------
   -- Global types --
   ------------------

   type WS_Access is access Wide_String;

   type Info is null record;

   type Node;
   type Link is access Node;
   type Node (Length : Positive) is
      record
         Value : Wide_String (1..Length);
         Next  : Link;
      end record;

   ----------------------
   -- Global variables --
   ----------------------

   Head     : Link;
   Cursor   : Link;
   Previous : Link;

   ---------
   -- Add --
   ---------

   -- Adds unit to the list, unless it is already there.
   -- Otherwise, add to the tail.
   -- It is OK to add new units while iterating.
   procedure Add (Unit : Wide_String) is
      Upper_Unit : constant Wide_String := To_Upper (Unit);
      Current    : Link := Head;
   begin
      if Current = null then
         Head     := new Node'(Upper_Unit'Length, Upper_Unit, null);
         Cursor   := Head;
         Previous := null;
         return;
      end if;

      loop
         if Current.Value = Upper_Unit then
            return;
         end if;
         exit when Current.Next = null;
         Current := Current.Next;
      end loop;

      -- Not found
      -- Current still points to the last node
      Current.Next := new Node'(Upper_Unit'Length, Upper_Unit, null);
      if Cursor = null then
         Cursor   := Current.Next;
         Previous := Current;
      end if;
   end Add;

   -----------
   -- Error --
   -----------

   procedure Raise_Specification_Error (Mess : String) is
      use Ada.Exceptions;
   begin
      Raise_Exception (Specification_Error'Identity, Mess);
   end Raise_Specification_Error;
   pragma No_Return (Raise_Specification_Error);

   ----------
   -- Free --
   ----------
   procedure Free is new Ada.Unchecked_Deallocation (Node, Link);

   --------------------
   -- Post_Procedure --
   --------------------

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info)
   is
      pragma Unreferenced (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (State);
  begin
      null;
   end Post_Procedure;

   ------------------------
   -- Pre_Procedure_With --
   ------------------------

   procedure Pre_Procedure_With (Element : in     Asis.Element;
                                 Control : in out Asis.Traverse_Control;
                                 State   : in out Info)
   is
      pragma Unreferenced (State);
      use Asis, Asis.Elements, Asis.Expressions, Asis.Compilation_Units;
      use Thick_Queries;

   begin
      Control := Continue;

      case Element_Kind (Element) is
         when A_Clause =>                 ------------ Clause ------------
            case Clause_Kind (Element) is
               when A_With_Clause =>
                  -- Let recurse into children
                  null;
               when others =>
                  -- Not interested
                  Control := Abandon_Children;
            end case;
         when An_Expression =>                 ------------ Expression ------------
            case Expression_Kind (Element) is
               when An_Identifier =>
                  -- Only identifiers from with clauses come here
                  -- However, we do not add predefined units
                  if Unit_Origin (Enclosing_Compilation_Unit
                                    (Corresponding_Name_Definition (Element))) =
                    An_Application_Unit
                  then
                     Add (Full_Name_Image (Corresponding_Name_Definition (Element)));
                  end if;
               when others =>
                  null;
            end case;

         when others =>
            -- Not interested
            Control := Abandon_Children;
      end case;

   exception
      when others =>
         Trace ("Exception in Pre-proc of Units_List ", Element); --## rule line off no_trace
         raise;
   end Pre_Procedure_With;

   -------------------
   -- Traverse_With --
   -------------------

   procedure Traverse_With is new Asis.Iterator.Traverse_Element
     (Info, Pre_Procedure_With, Post_Procedure);

   ----------------------------------------------------------------
   --                 Exported subprograms                       --
   ----------------------------------------------------------------

   ------------------
   -- Current_Unit --
   ------------------

   function Current_Unit return Wide_String is
   begin
      return Cursor.Value;
   end Current_Unit;

   --------------------
   -- Delete_Current --
   --------------------

   procedure Delete_Current is
      To_Free : Link := Cursor;
   begin
      if Previous = null then
         Head := Cursor.Next;
      else
         Previous.Next := Cursor.Next;
      end if;
      Cursor := Cursor.Next;
      Free (To_Free);
   end Delete_Current;

   ------------------
   -- Is_Exhausted --
   ------------------

   function Is_Exhausted return Boolean is
   begin
      return Cursor = null;
   end Is_Exhausted;

   --------------
   -- Register --
   --------------

   procedure Register (Unit_Spec  : in     Wide_String;
                       Recursive  : in     Boolean;
                       Add_Stubs  : in     Boolean;
                       My_Context : in out Asis.Context)
   is
      use Asis, Asis.Compilation_Units, Asis.Elements, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Maps;

      procedure Free is new Ada.Unchecked_Deallocation (Wide_String, WS_Access);

      Ignored_Units : array (1 .. Count (Unit_Spec, "-")    ) of WS_Access;
      Ignored_Inx   : Natural := 0;
      Separators    : constant Wide_Character_Set := To_Set ("+-");

      function Must_Ignore (Name : Wide_String) return Boolean is
         -- Check if unit name is either ignored, or a child (or a subunit) of an ignored unit
      begin
         for I in Ignored_Units'Range loop
            if Name = Ignored_Units (I).all or else
              (Name'Length > Ignored_Units (I).all'Length + 1 and then
               Ignored_Units (I).all = Name (Name'First .. Name'First+Ignored_Units (I).all'Length-1) and then
               Name (Name'First + Ignored_Units (I).all'Length) = '.')
            then
               return True;
            end if;
         end loop;
         return False;
      end Must_Ignore;

      procedure Do_Process_With (My_Unit : Compilation_Unit) is
         The_Control : Traverse_Control := Continue;
         The_Info    : Info;

      begin
         if Is_Nil (My_Unit) then
            return;
         end if;

         declare
            My_CC_List : constant Context_Clause_List
              := Context_Clause_Elements (Compilation_Unit => My_Unit,
                                          Include_Pragmas  => True) ;
         begin
            for I in My_CC_List'Range loop
               Traverse_With (My_CC_List (I), The_Control, The_Info);
            end loop;
         end;
      end Do_Process_With;

      procedure Do_Process_Stub (My_Unit : Compilation_Unit) is
      begin
         if Is_Nil (My_Unit) then
            return;
         end if;

         if Unit_Origin (My_Unit) /= An_Application_Unit or else
           Must_Ignore (To_Upper(Unit_Full_Name (My_Unit)))
         then
            return;
         end if;

         declare
            My_CU_List : constant Compilation_Unit_List := A4G_Bugs.Subunits (My_Unit);
         begin
            for I in My_CU_List'Range loop
               -- We do not add stubs is Add_Stubs is false, but we still need to
               -- add units that are withed by the stub
               if Add_Stubs then
                  Add (Unit_Full_Name (My_CU_List (I)));
               end if;
               Do_Process_With (My_CU_List (I));
            end loop;
         end;
      end Do_Process_Stub;

      -- Forward declaration:
      procedure Process_Unit_Spec (Unit_Spec : Wide_String);

      procedure Process_Indirect_File (Name : String) is
         use Ada.Wide_Text_IO, Ada.Strings,Ada.Strings.Wide_Fixed;

         Units_File : Ada.Wide_Text_IO.File_Type;

         function Read_Line return Wide_String is
            Buffer : Wide_String (1..250);
            Last   : Natural;
         begin
            Get_Line (Units_File, Buffer, Last);
            if Last = Buffer'Last then
               return Buffer & Read_Line;
            else
               return Buffer (1 .. Last);
            end if;
         end Read_Line;
      begin
         Open (Units_File, In_File, Name);

         -- Exit on End_Error
         -- This is the simplest way to deal with improperly formed files
         loop
            declare
               Line : constant Wide_String := Trim (Read_Line, Both);
            begin
               if Line /= "" and then Line (1) /= '#' then
                  Process_Unit_Spec (Line);
               end if;
            end;
         end loop;

         Close (Units_File);
      exception
         when Name_Error =>
            Raise_Specification_Error ("Missing units file: " & Name);
         when others =>  -- Including End_Error
            if Is_Open (Units_File) then
               Close (Units_File);
            end if;
      end Process_Indirect_File;

      procedure Process_Unit_Spec (Unit_Spec : Wide_String) is
         use Ada.Characters.Handling;

         Start : Positive;
         Stop  : Natural;
      begin
         --
         -- Get rid of case of indirect file:
         --
         if Unit_Spec (Unit_Spec'First) = '@' then
            if Unit_Spec'Length = 1 then
               -- '@' alone
               Raise_Specification_Error ("Missing file name after @");
            else
               Process_Indirect_File (To_String (Unit_Spec (Unit_Spec'First+1 .. Unit_Spec'Last)));
               return;
            end if;
         end if;

         --
         -- Extract unit names and ignored units from unit spec.
         --

         -- If Unit_spec starts with '-', our count is wrong...
         -- let's forbid this case
         if Unit_Spec (Unit_Spec'First) = '-' then
            Raise_Specification_Error ("Wrong unit specification: " & To_String (Unit_Spec));
         end if;

         if Unit_Spec (Unit_Spec'First) = '+' then
            Start := Unit_Spec'First + 1;
         else
            Start := Unit_Spec'First;
         end if;
         loop
            Stop := Index (Unit_Spec (Start .. Unit_Spec'Last), Separators);
            if Stop = 0 then
               Stop := Unit_Spec'Last;
            elsif Stop = Start then   -- "--" or "+-" or "-+" or "++" ...
               Raise_Specification_Error ("No unit name after '-' or '+'");
            else
               Stop := Stop - 1;
            end if;
            if Start = Unit_Spec'First or else Unit_Spec (Start-1) = '+' then
               Add (To_Upper(Unit_Spec (Start .. Stop)));
            else
               Ignored_Inx                 := Ignored_Inx + 1;
               Ignored_Units (Ignored_Inx) := new Wide_String'(To_Upper(Unit_Spec (Start .. Stop)));
            end if;
            exit when Stop = Unit_Spec'Last;
            Start := Stop + 2;
         end loop;
      end Process_Unit_Spec;

   begin  -- Register

      Process_Unit_Spec (Unit_Spec);

      --
      --  Process list of units
      --

      Reset;
      while not Is_Exhausted loop
         declare
            One_Unit : constant Wide_String := Current_Unit;
            Inx      : Natural;
         begin
            if Must_Ignore (One_Unit) then
               Delete_Current;
            else
               -- Search for subunits stubs
               -- Stubs can only appear in bodies
               Do_Process_Stub (Compilation_Unit_Body (One_Unit, My_Context));

               if Recursive then
                  -- Add parent if any
                  Inx := Index (One_Unit, ".");
                  if Inx /= 0 then
                     Add (One_Unit (One_Unit'First .. Inx-1));
                  end if;

                  -- Analyze with clauses
                  Do_Process_With (Library_Unit_Declaration (One_Unit, My_Context));
                  Do_Process_With (Compilation_Unit_Body (One_Unit, My_Context));
               end if;
               Skip;
            end if;
         end;
      end loop;

      for I in Ignored_Units'Range loop
         Free (Ignored_Units (I));
      end loop;
   end Register;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Cursor   := Head;
      Previous := null;
   end Reset;

   ----------
   -- Skip --
   ----------

   procedure Skip is
   begin
      Previous := Cursor;
      Cursor   := Cursor.Next;
   end Skip;

end Units_List;

