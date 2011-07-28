----------------------------------------------------------------------
--  Thick_Queries - Package body                                    --
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
  Ada.Strings.Wide_Fixed;

with   -- Reusable components
  A4G_Bugs;

with   -- ASIS units
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements,
  Asis.Text;
package body Thick_Queries is
   use Asis, Asis.Elements, Asis.Declarations, Asis.Statements;

   ------------------------------------------------------------------
   -- Internal utilities                                           --
   ------------------------------------------------------------------

   ----------------
   -- Impossible --
   ----------------

   procedure Impossible (Message : Wide_String; E : Asis.Element) is
      use Ada.Exceptions, Ada.Characters.Handling, Asis.Text;
      S : constant Span := Element_Span (E);
   begin
      Raise_Exception (Program_Error'Identity,
                       Message => To_String (Message) &
                         " at" & Line_Number'Image (S.First_Line) &
                         ":"   & Character_Position'Image (S.First_Column));
   end Impossible;
   pragma No_Return (Impossible);

   ------------------------------------------------------------------
   -- Exported subprograms                                         --
   ------------------------------------------------------------------

   --------------------
   -- Called_Profile --
   --------------------

   function Called_Profile (Call : Asis.Element) return Asis.Parameter_Specification_List is
      use Asis.Expressions, Asis.Definitions;
      Callee : Asis.Declaration;
      Name   : Asis.Expression;
   begin
      if Expression_Kind (Call) = A_Function_Call then
         Callee := A4G_Bugs.Corresponding_Called_Function (Call);
      else
         Callee := A4G_Bugs.Corresponding_Called_Entity (Call);
      end if;

      if Is_Nil (Callee) then
         -- Called subprogram not statically known
         if Expression_Kind (Call) = A_Function_Call then
            Name := Prefix (Call);
         else
            Name := Called_Name (Call);
         end if;

         if Expression_Kind (Name) = An_Explicit_Dereference then
            Name := Prefix (Name);
         end if;

         if Expression_Kind (Name) = A_Selected_Component then
            Name := Selector (Name);
         end if;

         if Expression_Type_Kind (Name) = An_Access_Type_Definition then
            -- access to subprogram (or entry)
            return Access_To_Subprogram_Parameter_Profile (Ultimate_Expression_Type (Name));

         else
            -- Dispatching Call...
            return Parameter_Profile (Corresponding_Name_Declaration (Name));
         end if;

      else
         if Declaration_Kind (Callee) in
           A_Procedure_Instantiation .. A_Function_Instantiation
         then
            Callee := Corresponding_Declaration (Callee);
         end if;

         return Parameter_Profile (Callee);
      end if;
   end Called_Profile;

   ----------------------------
   -- Enclosing_Program_Unit --
   ----------------------------

   function Enclosing_Program_Unit (Element          : Asis.Element;
                                    Including_Accept : Boolean      := False)
                                   return Asis.Defining_Name
   is
      -- A program unit has only one name
      Program_Unit_Name_Pos : constant := 1;

      My_Enclosing_Element : Asis.Element := Enclosing_Element(Element);

      Result : Asis.Defining_Name;

      use Asis.Declarations;
      use Asis.Statements;

   begin

      if Is_Nil (My_Enclosing_Element) then
         return Nil_Element;
      end if;

      loop
         case Element_Kind (My_Enclosing_Element) is
            when A_Declaration =>
               case Declaration_Kind (My_Enclosing_Element) is
                  when A_Procedure_Declaration
                    | A_Procedure_Body_Declaration
                    --
                    | A_Function_Declaration
                    | A_Function_Body_Declaration
                    --
                    | A_Package_Declaration
                    | A_Package_Body_Declaration
                    --
                    | A_Task_Type_Declaration
                    | A_Single_Task_Declaration
                    | A_Task_Body_Declaration
                    --
                    | A_Protected_Type_Declaration
                    | A_Single_Protected_Declaration
                    | A_Protected_Body_Declaration
                    --
                    | An_Entry_Declaration
                    | An_Entry_Body_Declaration
                    --
                    | A_Procedure_Body_Stub
                    | A_Function_Body_Stub
                    | A_Package_Body_Stub
                    | A_Task_Body_Stub
                    | A_Protected_Body_Stub
                    --
                    | A_Generic_Procedure_Declaration
                    | A_Generic_Function_Declaration
                    | A_Generic_Package_Declaration
                    =>
                     Result := Names(My_Enclosing_Element)(Program_Unit_Name_Pos);
                     exit;

                  when others =>
                     null;

               end case;

            when A_Statement =>
               case Statement_Kind(Element) is
                  when An_Accept_Statement =>
                     if Including_Accept then
                       Result := Names (Corresponding_Entry
                                        (My_Enclosing_Element)
                                       ) (Program_Unit_Name_Pos);
                       exit;
                     end if;

                  when others =>
                     null;

               end case;

            when others=>
               null;

         end case;

         My_Enclosing_Element := Enclosing_Element(My_Enclosing_Element);

      end loop;

      return Result;

   end Enclosing_Program_Unit;


   --------------------------
   -- Expression_Type_Kind --
   --------------------------

   function Expression_Type_Kind (The_Element : Asis.Expression) return Asis.Type_Kinds is
      The_Type : constant Asis.Declaration := Ultimate_Expression_Type (The_Element);
   begin
      if Is_Nil (The_Type) then
         return Not_A_Type_Definition;
      else
         return Type_Kind (The_Type);
      end if;
   end Expression_Type_Kind;


   -------------------------
   -- Extended_Name_Image --
   -------------------------

   function Extended_Name_Image (Name_Elem : Asis.Element) return Wide_String is
      use Asis, Asis.Elements, Asis.Expressions;
   begin
      case Expression_Kind (Name_Elem) is
         when A_Selected_Component =>
            return Extended_Name_Image (Prefix (Name_Elem)) & '.'
                   & Name_Image (Selector (Name_Elem));
         when An_Identifier =>
            return Name_Image (Name_Elem);
         when others =>
            Impossible ("Not a name in Extended_Name_Image", Name_Elem);
         end case;
   end Extended_Name_Image;


   ---------------------
   -- Full_Name_Image --
   ---------------------

   function Full_Name_Image (The_Name     : in Asis.Defining_Name;
                             With_Profile : in Boolean := False) return Wide_String is
      use Ada.Strings.Wide_Fixed, Asis.Compilation_Units;

      Parent          : Element;
      Anonymous_Count : Natural;
      Up_Count        : Natural;

      function Simple_Name_Image (N : Asis.Defining_Name) return Wide_String is
         -- Adds profile to name if necessary
         Name_Image : constant Wide_String := Defining_Name_Image (N);
      begin
         if With_Profile and then
           -- A generic is not overloadable, therefore we don't add the profile
           Declaration_Kind (Enclosing_Element (N)) not in A_Generic_Declaration
         then
            return Name_Image & Profile_Image (N, With_Profile => True);
         else
            return Name_Image;
         end if;
      end Simple_Name_Image;

   begin   -- Full_Name_Image

      -- Get rid of (annoying) special case:
      -- A defining name that is actually part of a Defining_Expanded_Name
      -- (i.e. the composite name of a child unit).
      -- The full name is actually the Defining_Name of the enclosing construct
      if Element_Kind (Enclosing_Element (The_Name)) = A_Defining_Name and then
        Defining_Name_Kind (Enclosing_Element (The_Name)) = A_Defining_Expanded_Name
      then
         return Simple_Name_Image (Enclosing_Element (The_Name));
      end if;

      -- Set Up_Count, the number of enclosing elements we have to go up
      -- to reach the real parent (the unit whose name is used to qualify The_Name)
      if Is_Part_Of_Implicit (The_Name) then
         if Defining_Name_Kind (The_Name) = A_Defining_Operator_Symbol then
            -- Implicit operator: (is this a A4G bug?), the definition is *not* enclosed in the
            -- derived type definition as in the normal case below
            Up_Count := 2;
         else
            --  Implicit element: The name is enclosed in a subprogram declaration, whose
            --  enclosing element is the derived type definition, whose enclosing element
            --  is the derived type declaration, whose enclosing element is the parent (Ouch).
            --  See the comment for Corresponding_Name_Definition in Asis.Expressions.
            Up_Count := 4;
         end if;
      elsif Declaration_Kind (Enclosing_Element (The_Name)) = An_Enumeration_Literal_Specification then
         -- Enumeration literal: The name is enclosed in an enumeration_literal_specification,
         -- whose enclosing element is the type definition, whose enclosing element is the type
         -- declaration, whose enclosing element is the parent (Ouch again).
         Up_Count := 4;
      else
         -- Normal case: The name is enclosed in a declaration or statement, whose enclosing
         -- element is the parent
         Up_Count := 2;
     end if;

     Parent := The_Name;
      for I in 1 .. Up_Count loop
         Parent := Enclosing_Element (Parent);
      end loop;

      -- There are cases of nested definitions => go up one (or more) levels
      -- until we find something that's the "real" parent.
      --
      -- We must skip declarations that are instantiations (i.e. if we have an instantiation of
      -- a generic package, the package declaration is enclosed in a package instantiation; the actual
      -- parent is the parent of the instantiation).
      -- Things we may encounter (and skip) are paths (they do not correspond to any (terminal) Ada
      -- element) and exception handlers (case of when E: others =>)
      -- If we encounter unnamed loops or blocks, we count them, but continue to go up. This allows
      -- generating a junk name that includes as many "_anonymous_." as unnamed statements

      Anonymous_Count := 0;
      loop
         case Element_Kind (Parent) is
            when A_Declaration =>
               case Declaration_Kind (Parent) is
                  when A_Generic_Instantiation =>
                     null;
                  when others =>
                     return Full_Name_Image (Names (Parent) (1), With_Profile) &
                       '.' &
                       Anonymous_Count * "_anonymous_." &
                       Simple_Name_Image (The_Name);
               end case;

            when Not_An_Element =>
               -- No parent => compilation unit
               -- But can still be a proper body
               if Unit_Kind (Enclosing_Compilation_Unit (The_Name)) in A_Subunit then
                  -- The full name is the same as the name of the stub
                  return Full_Name_Image (Names (Corresponding_Body_Stub
                                                 (Enclosing_Element (The_Name)))(1),
                                          With_Profile);
               else
                  return Simple_Name_Image (The_Name);
               end if;

            when A_Statement =>
               case Statement_Kind (Parent) is
                  when  A_Loop_Statement .. A_Block_Statement =>
                     -- Statements that can have a name
                     if Is_Nil (Statement_Identifier (Parent)) then
                        Anonymous_Count := Anonymous_Count + 1;
                     else
                        return Full_Name_Image (Statement_Identifier(Parent), With_Profile) &
                          '.' &
                          Anonymous_Count * "_anonymous_." &
                          Simple_Name_Image (The_Name);
                     end if;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Parent := Enclosing_Element (Parent);
      end loop;
   end Full_Name_Image;


   -----------------------
   -- Includes_Renaming --
   -----------------------

   function Includes_Renaming (Path : Asis.Expression) return Boolean is
      use Asis.Expressions;
   begin
      case Expression_Kind (Path) is
         when An_Identifier =>
            return Declaration_Kind (Corresponding_Name_Declaration (Path)) in A_Renaming_Declaration;
         when A_Selected_Component =>
            return Includes_Renaming (Selector (Path)) or else Includes_Renaming (Prefix (Path));
         when A_Function_Call | An_Indexed_Component | A_Slice =>
            return Includes_Renaming (Prefix (Path));
         when An_Explicit_Dereference =>
            return Includes_Renaming (Prefix (Path));
         when A_Type_Conversion | A_Qualified_Expression =>
            return Includes_Renaming (Converted_Or_Qualified_Subtype_Mark (Path));
         when others =>
            Impossible ("Includes_Renaming called on " &
                          Expression_Kinds'Wide_Image (Expression_Kind (Path)),
                        Path);
      end case;
   end Includes_Renaming;


   ---------------------------
   -- Is_Callable_Construct --
   ---------------------------

   function Is_Callable_Construct (Element : Asis.Element) return Boolean is
      use Asis.Expressions;
      The_Declaration : Asis.Element;
   begin
      -- Go to the declaration
      case Element_Kind (Element) is
         when A_Declaration =>
            The_Declaration := Element;
         when A_Defining_Name | A_Definition =>
            The_Declaration := Enclosing_Element (Element);
            if Declaration_Kind (The_Declaration) in A_Generic_Instantiation then
               -- For a generic instantiation, take the declaration from the generic
               -- Beware: the generic name can be a selected component
               The_Declaration := Generic_Unit_Name (The_Declaration);
               if Expression_Kind (The_Declaration) = A_Selected_Component then
                  The_Declaration := Selector (The_Declaration);
               end if;
               The_Declaration := Corresponding_Name_Declaration (The_Declaration);
            end if;
         when others =>
            -- Impossible
            Impossible ("Is_Callable_Construct called on " &
                          Element_Kinds'Wide_Image (Element_Kind (Element)),
                        Element);
      end case;

      case Declaration_Kind (The_Declaration) is
         when
           A_Procedure_Declaration |
           A_Procedure_Body_Declaration |
           A_Procedure_Renaming_Declaration |
           A_Procedure_Body_Stub |
           A_Generic_Procedure_Declaration |
           A_Formal_Procedure_Declaration |
           --
           A_Function_Declaration |
           A_Function_Body_Declaration |
           A_Function_Renaming_Declaration |
           A_Function_Body_Stub |
           A_Generic_Function_Declaration |
           A_Formal_Function_Declaration |
           --
           An_Enumeration_Literal_Specification |
           --
           An_Entry_Declaration |
           An_Entry_Body_Declaration =>

            return True;
         when others =>
            return False;
      end case;
   end Is_Callable_Construct;


   -------------------
   -- Profile_Image --
   -------------------

   function Profile_Image (The_Name : Asis.Defining_Name; With_Profile : Boolean := True)
                          return Wide_String
   is
      function Entry_Name (The_Entry : Profile_Entry) return Wide_String is
         function Add_Attribute (S : Wide_String) return Wide_String is
         begin
            case The_Entry.Attribute is
               when None =>
                  return S;
               when Base =>
                  return S & "'BASE";
               when Class =>
                  return S & "'CLASS";
            end case;
         end Add_Attribute;

      begin
         if The_Entry.Is_Access then
            return '*' & Add_Attribute (Full_Name_Image (The_Entry.Name, With_Profile));
         else
            return Add_Attribute (Full_Name_Image (The_Entry.Name, With_Profile));
         end if;
      end Entry_Name;

      function Build_Names (The_List : Profile_Table) return Wide_String is
      begin
         if The_List'Length = 0 then
            return "";
         elsif The_List'Length = 1 then
            return Entry_Name (The_List (The_List'First));
         else
            return Entry_Name (The_List (The_List'First)) &
              ';' &
              Build_Names (The_List (The_List'First+1..The_List'Last));
         end if;
      end Build_Names;

   begin
      if Is_Callable_Construct (The_Name) then
         declare
            Profile_Names : constant Profile_Descriptor := Types_Profile (Enclosing_Element (The_Name));
         begin
            if Is_Nil (Profile_Names.Result_Type.Name) then
               -- A procedure (or entry...)
               return '{' & Build_Names (Profile_Names.Formals) & '}';
            else
               -- A function
               return '{' & Build_Names (Profile_Names.Formals) & ':' &
                 Entry_Name (Profile_Names.Result_Type) & '}';
            end if;
         end;
      else
         return "";
      end if;
   end Profile_Image;

   -------------------
   -- Types_Profile --
   -------------------

   function Types_Profile (Declaration : in Asis.Declaration)  return Profile_Descriptor is
      use Asis.Expressions;

      function Build_Entry (Mark : Asis.Element) return Profile_Entry is
         -- To be honnest, builds the entry except the Is_Access field
         Good_Mark : Asis.Element;
         Attribute : Type_Attribute;
      begin
         if Expression_Kind (Mark) = An_Attribute_Reference then
            Good_Mark := Prefix (Mark);

            case A4G_Bugs.Attribute_Kind (Mark) is
               when A_Base_Attribute =>
                  Attribute := Base;
               when A_Class_Attribute =>
                  Attribute := Class;
               when others =>
                  -- Impossible
                  Impossible ("Attribute of Type_Profile = "
                                & Attribute_Kinds'Wide_Image (A4G_Bugs.Attribute_Kind (Mark)),
                              Declaration);
            end case;

         else
            Good_Mark := Mark;
            Attribute := None;
         end if;

         if Expression_Kind (Good_Mark) = A_Selected_Component then
            -- The subtype Mark is as given, so it can be either a name or
            -- a selected component.
            Good_Mark := Selector (Good_Mark);
         end if;

         return (Is_Access => False,
                 Attribute => Attribute,
                 Name      => Names (Corresponding_First_Subtype
                                       (Corresponding_Name_Declaration
                                          (Good_Mark)))(1));
      end Build_Entry;

      function Build_Profile (Parameters : Parameter_Specification_List) return Profile_Table is
         -- Assert: parameters is not an empty list
         -- This function is written to avoid recursivity if there is no other multiple
         -- parameter declaration than the first one.

         Names_1    : constant Name_List := Names (Parameters (Parameters'First));
         Entry_1    : Profile_Entry := Build_Entry (Declaration_Subtype_Mark
                                                      (Parameters (Parameters'First)));
         Result     : Profile_Table (1 .. Names_1'Length + Parameters'Length -1);
         Result_Inx : Asis.List_Index;
      begin
         Entry_1.Is_Access := Trait_Kind (Parameters (Parameters'First)) = An_Access_Definition_Trait;
         for I in 1 .. Names_1'Length loop
            Result (I) := Entry_1;
         end loop;

         Result_Inx := Names_1'Length;
         for I in Parameters'First + 1 .. Parameters'Last loop
            declare
               Names_Rest : constant Name_List := Names (Parameters (I));
            begin
               if Names_Rest'Length = 1 then
                  Result_Inx := Result_Inx + 1;
                  Result (Result_Inx) := Build_Entry (Declaration_Subtype_Mark (Parameters (I)));
                  Result (Result_Inx).Is_Access := Trait_Kind (Parameters (I)) = An_Access_Definition_Trait;
               else
                  return Result (1 .. Result_Inx) &
                    Build_Profile (Parameters (I .. Parameters'Last));
               end if;
            end;
         end loop;
         return Result;
      end Build_Profile;

      Result_Entry     : Profile_Entry;
      Good_Declaration : Asis.Declaration := Declaration;
   begin  --  Types_Profile
      if Declaration_Kind (Good_Declaration) in A_Generic_Instantiation then
         -- We must get the profile from the corresponding generic element
         Good_Declaration := Corresponding_Declaration (Good_Declaration);
      end if;

      case Declaration_Kind (Good_Declaration) is
         when A_Function_Declaration |
           A_Function_Body_Declaration |
           A_Function_Renaming_Declaration |
           A_Function_Body_Stub |
           A_Generic_Function_Declaration |
           A_Formal_Function_Declaration =>

            Result_Entry := Build_Entry (Result_Profile (Good_Declaration));

         when An_Enumeration_Literal_Specification =>
            -- Profile for an enumeration litteral
            -- Like a parameterless function; go up two levels (type specification then type declaration)
            -- to find the return type.
            -- of the Enumaration_Literal_Specification
            -- Return immediately, since we know there are no parameters, and Parameter_Profile
            -- would choke on this.
            return (Formals_Length => 0,
                    Result_Type    => (Is_Access => False,
                                       Attribute => None,
                                       Name      => Names (Enclosing_Element
                                                             (Enclosing_Element
                                                                (Good_Declaration)))(1)),
                    Formals        => (others => (False, None, Nil_Element)));
         when others =>
            Result_Entry := (Is_Access => False,
                             Attribute => None,
                             Name      => Nil_Element);
      end case;

      declare
         Parameters : constant Asis.Parameter_Specification_List := Parameter_Profile (Good_Declaration);
      begin
         if Parameters'Length = 0 then
            return (Formals_Length => 0,
                    Result_Type    => Result_Entry,
                    Formals        => (others => (False, None, Nil_Element)));
         else
            declare
               Profile : constant Profile_Table := Build_Profile (Parameters);
            begin
               return (Formals_Length => Profile'Length,
                       Result_Type    => Result_Entry,
                       Formals        => Profile);
            end;
         end if;
      end;
   end Types_Profile;

   ------------------------------
   -- Ultimate_Expression_Type --
   ------------------------------

   function Ultimate_Expression_Type (The_Element : Asis.Expression) return Asis.Declaration is
      use Asis.Definitions;

      Local_Elem : Asis.Element := A4G_Bugs.Corresponding_Expression_Type (The_Element);
   begin
      if Is_Nil (Local_Elem) then
         -- The_Element is a package, subprogram, task...
         -- TBSL tasks
         return Nil_Element;
      end if;
      Local_Elem := Type_Declaration_View (Corresponding_First_Subtype (Local_Elem));

      if Definition_Kind (Local_Elem) in
        A_Private_Type_Definition .. A_Private_Extension_Definition
      then
         Local_Elem := Type_Declaration_View (Corresponding_First_Subtype
                                                (Corresponding_Type_Declaration
                                                   (Enclosing_Element (Local_Elem))));
      end if;

      if Type_Kind (Local_Elem) in
        A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
      then
         Local_Elem := Type_Declaration_View (Corresponding_Root_Type (Local_Elem));
      end if;

      return Local_Elem;
   end Ultimate_Expression_Type;

end Thick_Queries;

