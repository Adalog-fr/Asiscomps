----------------------------------------------------------------------
--  Thick_Queries - Package specification                           --
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

with Asis;
package Thick_Queries is

   function Enclosing_Program_Unit (Element          : Asis.Element;
                                    Including_Accept : Boolean      := False)
                                   return Asis.Defining_Name;
   --  Return the Defining_Name of the innermost enclosing program unit of any Element
   --  If Including_Accept is true and the element is within an accept statement, return
   --  the corresponding entry name (not really a program unit, but useful f.e. if the
   --  Element is a Return_Statement, and you want to know what you are returning from).
   --
   --  Appropriate Element_Kinds:
   --     Any element
   --
   --  Returns
   --     A_Defining_Name
   --     Nil_Element if Element is a Compilation_Unit
   --


   function Attribute_Name_Image (Attribute : Asis.Expression) return Wide_String;
   --  Like Pragma_Name_Image, but for an Attribute_Reference
   --
   --  Appropriate Element_Kinds:
   --    An_Expression
   --  Appropriate Expression_Kinds:
   --    An_Attribute_Reference


   function Extended_Name_Image (Name_Elem : Asis.Element) return Wide_String;
   -- Image of a name, given either as a simple name or as a Selected_Name


   function Full_Name_Image (The_Name     : in Asis.Element;
                             With_Profile : in Boolean := False) return Wide_String;
   -- Full name of a name
   -- Works like Asis.Declarations.Defining_Name_Image (or Name_Image),
   -- but returns the full (unique) name of The_Name, starting from the
   -- enclosing compilation unit (Standard for predefined elements).
   -- If With_Profile is true, "mangles" the name with a profile to provide a name
   -- that is unique even if overloaded.
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (returns the image of the selector)


   function Profile_Image (The_Name     : Asis.Element;
                           With_Profile : Boolean := True) return Wide_String;
   -- Image of the profile of a callable construct
   -- If name is not a callable construct, returns ""
   -- Otherwise:
   --    for a procedure, entry...:
   --       returns '{' {<Full_Name_Image (types of parameter) '}'}
   --    for a function:
   --       returns '{' {<Full_Name_Image (types of parameter) } ':' Full_Name_Image (result type) '}'
   -- With_Profile determines if the Full_Name_Image generated for parameters and result includes
   -- itself a profile.
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (returns the image of the selector)


   function Is_Class_Wide_Subtype (The_Subtype : Asis.Declaration) return Boolean;
   -- Unwinds subtype declarations and returns true if the given subtype declaration
   -- ultimately designates a class-wide type.
   --
   -- Appropriate Declaration_Kinds:
   --    A_Subtype_Declaration

   function Ultimate_Expression_Type (The_Element : Asis.Expression) return Asis.Declaration;
   -- return the type declaration of the ultimate ancestor type of The_Element
   -- (going up all subtype and derived type declaration).


   function Ultimate_Name (The_Name : Asis.Element) return Asis.Element;
   -- For a name defined by a renaming declaration: returns the name of the entity, which is not
   --   itself defined by a renaming.
   --   - In the case of a renaming whose target is part of a composite type, returns the name
   --     of the field for A_Selected_Name and the name of the array for An_Indexed_Component
   --        (i.e. X : T renames V.Field (2) => Field).
   --   - In the case of a renaming whose target is An_Explicit_Dereference, returns Nil_Element
   --     (the target is statically unknown)
   --   - In the case of a renaming whose target is A_Function_Call, returns Nil_Element
   --     (the target is statically unknown, it designates the result of the function call)
   --   - In the case of the renaming of an attribute, returns the attribute
   -- Otherwise: returns its argument
   --
   -- In any case, if The_Name is A_Defining_Name, then A_Defining_Name is returned
   -- if The_Name is An_Identifier, then An_Identifier (or An_Attribute_Reference) is returned
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    An_Attribute_Reference

   function Ultimate_Enclosing_Instantiation (The_Element : Asis.Element) return Asis.Declaration;
   -- For an entity which Is_Part_Of_Instance:
   -- Return the "true" instantiation, i.e. the one written by the user, going up instantiations
   -- that appear in generics.

   function Expression_Type_Kind (The_Element : Asis.Expression) return Asis.Type_Kinds;
   -- Real kind of an expression
   -- return the Type_Kind of the ultimate ancestor of The_Element
   -- (going up all subtype and derived type declaration).

   function Called_Simple_Name (Call : Asis.Element) return Asis.Expression;
   -- Given a procedure, entry or function call, returns the simple name of the called
   -- entity.
   -- It handles all cases in the same function (i.e. whether it is a procedure or a function,
   -- whether the call is from an access to subprogram, etc.)
   -- Returns the simple name of the called entity (i.e. not a Selected_Name).
   -- For calls to an entry family, returns the name of the family
   -- Returns a Nil_Element if the call is through an access to subprogram
   -- Works with dispatching calls!
   --
   -- Appropriate Element_Kinds:
   --    A_Statement
   --    An_Expression
   -- Appropriate Statement_Kinds:
   --    A_Procedure_Call_Statement
   --    An_Entry_Call_Statement
   -- Appropriate Expression_Kinds:
   --    A_Function_Call

   function Called_Profile (Call : Asis.Element) return Asis.Parameter_Specification_List;
   -- Given a procedure, entry or function call, returns the parameter profile of the called
   -- entity.
   -- For simple cases, it is like Parameter_Profile (Corresponding_Called_Entity (Call)),
   -- but it handles all cases in the same function (i.e. whether it is a procedure or a function,
   -- whether the call is from an access to subprogram, etc.)
   --
   -- Appropriate Element_Kinds:
   --    A_Statement
   --    An_Expression
   -- Appropriate Statement_Kinds:
   --    A_Procedure_Call_Statement
   --    An_Entry_Call_Statement
   -- Appropriate Expression_Kinds:
   --    A_Function_Call

   function Actual_Parameters (Element : Asis.Element; Normalized : Boolean := False) return Asis.Association_List;
   -- Returns the actual parameters of a procedure, entry, or function call, or of
   -- a generic instantiation

   function Formal_Name (Call : Asis.Element; Actual : Asis.List_Index)
                        return Asis.Defining_Name;
   -- Given a procedure, entry or function call, or a generic instantiation, returns the defining name
   -- of the formal corresponding to the actual at the given position in the call.
   -- Note: if the full Parameter_Specification is desired, it is the Enclosing_Element of the Formal_Name
   --
   -- Returns a nil element if:
   --   Actual is greater than the number of actuals in the call
   --   The call is to a dispatching operation


   type Type_Attribute is (None, Base, Class);
   type Profile_Entry is
      record
         Is_Access : Boolean;
         Attribute : Type_Attribute;
         Name      : Asis.Defining_Name;
      end record;
   type Profile_Table is array (Asis.List_Index range <>) of Profile_Entry;
   type Profile_Descriptor (Formals_Length : Asis.ASIS_Natural) is
      record
         Result_Type : Profile_Entry;
         Formals     : Profile_Table (1..Formals_Length);
      end record;
   function Types_Profile (Declaration : in Asis.Declaration) return Profile_Descriptor;
   -- Given a callable entity declaration, returns a list of Defining_Name
   -- First element is the result type for a function, Nil_Element for other declarations
   -- Other elements are (in order of declaration) the *types* of the parameters.
   -- Multiple declarations are separated, i.e. "A,B : Integer" yields two entries in the table.


   function Includes_Renaming (Path : Asis.Expression) return Boolean;
   -- Checks whether any element in the Path is a renaming
   --
   -- Appropriate expression kinds:
   --   An_Identifier
   --   A_Selected_Component
   --   A_Function_Call
   --   An_Indexed_Component
   --   A_Slice


   function Is_Callable_Construct (Element : Asis.Element) return Boolean;
   -- Checks whether the Element is a callable construct
   -- Expected elements:
   --    A_Declaration
   --    A_Definition
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (applies to the selector)

   function Is_Part_Of_Generic (Declaration : in Asis.Declaration) return Boolean;
   -- Checks whether the Declaration is included (directly or indirectly) in a generic
   -- Expected Element:
   --    A_Declaration
end Thick_Queries;
