----------------------------------------------------------------------
--  A4G_Bugs - Package body                                         --
--  Copyright (C) 2005 Adalog                                       --
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

--## RULE OFF Use_A4G_Bugs ## We obviously need to call the original function here

with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Exceptions;

with   -- ASIS
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Exceptions,
  Asis.Expressions,
  Asis.Statements;

with   -- Adalog
  Thick_Queries,
  Utilities;   -- Only for Trace_Bug

package body A4G_Bugs is
   use Asis;

   -------------------------------------
   -- Attribute_Designator_Identifier --
   -------------------------------------

   function Attribute_Designator_Identifier (Expression : in Asis.Expression)
                                            return Asis.Expression
   is
      use Asis.Declarations, Asis.Elements, Ada.Characters.Handling, Ada.Exceptions;
      E : Asis.Element;
   begin
      if not Is_Part_Of_Implicit (Expression) then
         -- No bug in this case
         return Asis.Expressions.Attribute_Designator_Identifier (Expression);
      end if;

      -- We assume that the attribute is a 'Class or 'Base in the subtype indication of
      -- a formal parameter of a derived subprogram.
      -- Go up to the ancestor declaration, retrieve the corresponding formal, and get the
      -- Attribute_Designator_Identifier from its subtype indication
      E := Enclosing_Element (Expression);
      if Declaration_Kind (E) /= A_Parameter_Specification then
         Raise_Exception (Program_Error'Identity,
                          "Implicit attribute not in formal association "
                            & Declaration_Kinds'Image (Declaration_Kind (E)));
      end if;

      declare
         Name : constant Wide_String := Defining_Name_Image (Names (E)(1));
      begin
         E := Enclosing_Element (E);  -- Go to subprogram declaration
         while Is_Part_Of_Inherited (E) loop
            E := Corresponding_Subprogram_Derivation (E);
         end loop;

         declare
            Formals : constant Parameter_Specification_List := Parameter_Profile (E);
         begin
            for I in Formals'Range loop
               if Defining_Name_Image (Names (Formals (I))(1)) = Name then
                  return Asis.Expressions.Attribute_Designator_Identifier (Declaration_Subtype_Mark
                                                                             (Formals (I)));
               end if;
            end loop;
            Raise_Exception (Program_Error'Identity,
                             "Did not retrieve formal " & To_String (Name));
         end;
      end;
   end Attribute_Designator_Identifier;

   --------------------
   -- Attribute_Kind --
   --------------------

   Is_Vowel  : constant array (Character) of Boolean
     := ('A'|'a' | 'E'|'e' | 'I'|'i' | 'O'|'o' | 'U'|'u' | 'Y'|'y' => True,
         others => False);

   function Attribute_Kind (Expression : in Asis.Expression) return Asis.Attribute_Kinds is
      use Ada.Characters.Handling, Asis.Elements;
   begin
      if Expression_Kind (Expression) /= An_Attribute_Reference then
         return Not_An_Attribute;
      end if;

      declare
         Attribute_Name : constant Wide_String := Thick_Queries.Attribute_Name_Image (Expression);
      begin
         if Is_Vowel (To_Character (Attribute_Name (Attribute_Name'First))) then
            return Attribute_Kinds'Wide_Value ("An_" & Attribute_Name & "_Attribute");
         else
            return Attribute_Kinds'Wide_Value ("A_" & Attribute_Name & "_Attribute");
         end if;
      exception
         when Constraint_Error =>
            return An_Implementation_Defined_Attribute; -- We may be cheating here...
      end;
   end Attribute_Kind;

   ---------------------------------
   -- Corresponding_Called_Entity --
   ---------------------------------

   function Corresponding_Called_Entity (Statement : in Asis.Statement) return Asis.Declaration is
      use Asis.Elements;
      Result : constant Asis.Declaration := Asis.Statements.Corresponding_Called_Entity (Statement);
   begin
      if Defining_Name_Kind (Result) = A_Defining_Expanded_Name then
         -- Bug: missing one level of Enclosing_Element
         Trace_Bug ("A4G_Bugs.Corresponding_Called_Entity");
         return Enclosing_Element (Result);
      else
         return Result;
      end if;
   end Corresponding_Called_Entity;

   -----------------------------------
   -- Corresponding_Called_Function --
   -----------------------------------

   function Corresponding_Called_Function (Expression : in Asis.Expression) return Asis.Declaration is
      use Asis.Declarations, Asis.Elements;
      Result : constant Asis.Declaration := Asis.Expressions.Corresponding_Called_Function (Expression);
   begin
      -- Special kludge for "/=" which does not return Nil_Element in some version of GNAT
      -- (although Called_Profile does not return anything)
      if Is_Nil (Result) then
         return Nil_Element;
      elsif Declaration_Kind (Result) = A_Function_Instantiation then
         -- certainly not a predefined operator
         return Result;
      elsif Operator_Kind (Names (Result)(1)) = A_Not_Equal_Operator
        and then not Is_Nil (Corresponding_Equality_Operator (Result))
      then
         -- It is a predefined "/=" whose result is Boolean
         -- Note that it can only be a predefined one, per 6.6(5)
         return Nil_Element;
      end if;

      if Defining_Name_Kind (Result) = A_Defining_Expanded_Name then
         -- Bug: missing one level of Enclosing_Element
         Trace_Bug ("A4G_Bugs.Corresponding_Called_Function");
         return Enclosing_Element (Result);
      else
         return Result;
      end if;
   end Corresponding_Called_Function;

   -------------------------
   -- Corresponding_Entry --
   -------------------------

   function Corresponding_Entry (Statement : in Asis.Statement) return Asis.Declaration is
      use Asis.Statements;
   begin
      return Corresponding_Name_Declaration (Accept_Entry_Direct_Name (Statement));
   end Corresponding_Entry;

   -----------------------------------
   -- Corresponding_Expression_Type --
   -----------------------------------

   function Corresponding_Expression_Type (Expression : in Asis.Expression) return Asis.Declaration
   is
      use Ada.Exceptions, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      Result : Asis.Element;
      Temp   : Asis.Element;
   begin
      Result := Asis.Expressions.Corresponding_Expression_Type (Expression);

      if Is_Nil (Result) then
         -- There are cases where Corresponding_Expression_Type returns a wrong Nil result
         -- for the selector of a Selected_Component, but is OK on the Selected_Component itself.
         -- Let's give it a chance...
         if Expression_Kind (Enclosing_Element (Expression)) = A_Selected_Component
           and then Is_Equal (Expression, Selector (Enclosing_Element (Expression)))
         then
            Result := Asis.Expressions.Corresponding_Expression_Type (Enclosing_Element (Expression));
         end if;
         if Is_Nil (Result) then
            -- Must be true this time...
            return Result;
         end if;
         Trace_Bug ("A4G_Bugs.Corresponding_Expression_Type (1)");
      end if;

      case Declaration_Kind (Result) is
         when A_Component_Declaration =>
            -- Bug
            Trace_Bug ("A4G_Bugs.Corresponding_Expression_Type (2)");
            Result := Corresponding_Name_Declaration (Subtype_Simple_Name
                                                      (Component_Subtype_Indication
                                                       (Object_Declaration_View (Result))));

         when A_Type_Declaration | A_Subtype_Declaration | A_Formal_Type_Declaration =>
            -- OK
            null;

         when A_Variable_Declaration | A_Constant_Declaration | A_Deferred_Constant_Declaration =>
            -- Bug
            Trace_Bug ("A4G_Bugs.Corresponding_Expression_Type (3)");
            Result := Object_Declaration_View (Result);
            if Definition_Kind (Result) = A_Type_Definition then
               -- The object is declared with an anonymous type
               -- => there is no corresponding declaration
               return Nil_Element;
            end if;

            Result := Corresponding_Name_Declaration (Subtype_Simple_Name (Result));

         when Not_A_Declaration =>
            Raise_Exception (Program_Error'Identity,
                             "Bug in Corresponding_Expression_Type, returned "
                               & Element_Kinds'Image (Element_Kind (Result)));
         when others =>
            Raise_Exception (Program_Error'Identity,
                             "Bug in Corresponding_Expression_Type, returned "
                               & Declaration_Kinds'Image (Declaration_Kind (Result)));
      end case;
      return Result;
   end Corresponding_Expression_Type;

   --------------------------------
   -- Corresponding_Last_Subtype --
   --------------------------------

   function Corresponding_Last_Subtype (Declaration : in Asis.Declaration) return Asis.Declaration is
      -- Since this is a complete rewriting of the function, there is no call to Trace_Bug
      -- (and since the bug is an endless loop, there is no way to diagnose it)
      use Asis.Declarations, Asis.Elements, Asis.Expressions, Ada.Exceptions;
      use Thick_Queries;
      Mark : Asis.Expression;
   begin
      if Declaration_Kind (Declaration) /= A_Subtype_Declaration then
         return Declaration;
      end if;

      Mark := Subtype_Simple_Name (Type_Declaration_View (Declaration));
      case Expression_Kind (Mark) is
         when An_Identifier =>
            return Corresponding_Name_Declaration (Mark);
         when An_Attribute_Reference =>
            return Corresponding_Name_Declaration (Simple_Name (Prefix (Mark)));
         when others =>
            Raise_Exception (Program_Error'Identity,
                             "Bug in Corresponding_Last_Subtype, returned "
                               & Expression_Kinds'Image (Expression_Kind (Mark)));
      end case;
   end Corresponding_Last_Subtype;


   ------------------------------------
   -- Corresponding_Name_Declaration --
   ------------------------------------

   function Corresponding_Name_Declaration (Reference : in Asis.Expression) return Asis.Element is
      use Asis.Elements;
      Result : Asis.Element := Asis.Expressions.Corresponding_Name_Declaration (Reference);
   begin
      case Element_Kind (Result) is
         when Not_An_Element
            | A_Declaration
            | A_Statement
              =>
            return Result;
         when others =>
            -- Bug
            Trace_Bug ("A4G_Bugs.Corresponding_Name_Declaration");
            while Element_Kind (Result) /= A_Declaration loop
               Result := Enclosing_Element (Result);
            end loop;
            return Result;
      end case;
   end Corresponding_Name_Declaration;


   -----------------------------
   -- Corresponding_Root_Type --
   -----------------------------

   function Corresponding_Root_Type (Type_Definition : in Asis.Type_Definition) return Asis.Declaration is
      -- Since this is a complete rewriting of the function, there is no call to Trace_Bug
      use Asis.Definitions, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      Def  : Asis.Definition := Type_Definition;
      Decl : Asis.Declaration;
      Name : Asis.Expression;
   begin
      loop
         -- Invariant: Def is A_Derived_Type_Definition or A_Derived_Record_Extension_Definition

         Name := Subtype_Simple_Name (Parent_Subtype_Indication (Def));
         if Expression_Kind (Name) = An_Attribute_Reference then
            Name := Prefix (Name);
            if Expression_Kind (Name) = A_Selected_Component then
               Name := Selector (Name);
            end if;
         end if;

         Decl := Corresponding_First_Subtype (Corresponding_Name_Declaration (Name));
         if Declaration_Kind (Decl) /= An_Ordinary_Type_Declaration then
            return Decl;
         end if;

         Def := Type_Declaration_View (Decl);
         if Type_Kind (Def) not in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition then
            return Decl;
         end if;
      end loop;
   end Corresponding_Root_Type;

   ----------------
   -- Name_Image --
   ----------------

   function Name_Image (Expression : Asis.Expression) return Asis.Program_Text is
      use Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Exceptions;
      Def : Asis.Defining_Name;
   begin
      return Asis.Expressions.Name_Image (Expression);
   exception
      when ASIS_Failed =>
         -- This work-around seems OK:
         Trace_Bug ("A4G_Bugs.Name_Image");
         Def := Corresponding_Name_Definition (Expression);
         if Defining_Name_Kind (Def) = A_Defining_Expanded_Name then
            Def := Defining_Selector (Def);
         end if;
         return Defining_Name_Image (Def);
   end Name_Image;

   ---------------------
   -- Renamed_Entity --
   ---------------------

   function Renamed_Entity (Declaration : in Asis.Declaration) return Asis.Expression is
      use Asis.Elements, Asis.Expressions, Asis.Statements;
      Ren : constant Asis.Expression := Asis.Declarations.Renamed_Entity (Declaration);
   begin
      case A_Renaming_Declaration (Declaration_Kind (Declaration)) is
         when An_Object_Renaming_Declaration
            | An_Exception_Renaming_Declaration
            | A_Package_Renaming_Declaration
            | A_Generic_Package_Renaming_Declaration
            | A_Generic_Procedure_Renaming_Declaration
            | A_Generic_Function_Renaming_Declaration
              =>
            -- No problem here
            return Ren;
         when A_Procedure_Renaming_Declaration =>
            if Statement_Kind (Ren) = A_Procedure_Call_Statement then
               Trace_Bug ("A4G_Bugs.Renamed_Entity");
               return Called_Name (Ren);
            else
               return Ren;
            end if;
         when A_Function_Renaming_Declaration =>
            if Expression_Kind (Ren) = A_Function_Call then
               Trace_Bug ("A4G_Bugs.Renamed_Entity");
               return Prefix (Ren);
            else
               return Ren;
            end if;
      end case;
   end Renamed_Entity;

   --------------
   -- Subunits --
   --------------

   function Subunits (Parent_Body : in Asis.Compilation_Unit) return Asis.Compilation_Unit_List is
      use Asis.Compilation_Units, Asis.Declarations, Asis.Elements;

      -- Stubs can be declared only as top-level declarative items
      -- It is therefore sufficient to parse the declarations of the body (no need to recurse),
      -- and the number of stubs is <= the number of declarative items

      All_Declarations : constant Asis.Element_List
        := Body_Declarative_Items (Unit_Declaration (Parent_Body),
                                   Include_Pragmas => False);
      Stubs_List : Asis.Compilation_Unit_List (All_Declarations'Range);
      Stub_Index : ASIS_Integer := Stubs_List'First-1;

   begin
      for I in All_Declarations'Range loop
         if Declaration_Kind (All_Declarations (I)) in A_Body_Stub then
            declare
               The_Names : constant Asis.Defining_Name_List := Names (All_Declarations (I));
               The_Name  : Asis.Defining_Name renames The_Names (The_Names'First);
            begin
               Stub_Index := Stub_Index + 1;
               Stubs_List (Stub_Index) := Compilation_Unit_Body (Unit_Full_Name (Parent_Body) &
                                                                 '.' &
                                                                 Defining_Name_Image (The_Name),
                                                                 Enclosing_Context (Parent_Body));
            end;
         end if;
      end loop;

      if Stub_Index < Stubs_List'First then
         return Nil_Compilation_Unit_List;
      else
         return Stubs_List (Stubs_List'First .. Stub_Index);
      end if;
   end Subunits;

   ----------------
   -- Unit_Class --
   ----------------

   function Unit_Class (Compilation_Unit : in Asis.Compilation_Unit) return Asis.Unit_Classes is
      use Asis.Compilation_Units;
      Result : constant Asis.Unit_Classes := Asis.Compilation_Units.Unit_Class (Compilation_Unit);
   begin
      if Result = A_Public_Body and then Is_Nil (Corresponding_Declaration (Compilation_Unit)) then
         Trace_Bug ("A4G_Bugs.Unit_Class");
         return A_Public_Declaration_And_Body;
      else
         return Result;
      end if;
   end Unit_Class;

   ---------------
   -- Trace_Bug --
   ---------------

   procedure Trace_Bug (Message : Wide_String) is
      use Utilities;
   begin
      Trace ("ASIS bug workaround triggered in " & Message);  --## RULE LINE OFF No_Trace
   end Trace_Bug;

end A4G_Bugs;
