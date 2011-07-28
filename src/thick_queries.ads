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

   --  Return the Defining_Name of the innermost enclosing program unit of any Element
   --  If Including_Accept is true and the element is within an accept statement, return
   --  the corresponding entry name (not really a program unit, but useful f.e. if the
   --  Element is a Return_Statement, and you want to know what you are returning from).
   --
   --  Appropriate Element_Kinds:
   --     Any element
   --
   --
   --  Returns
   --     A_Defining_Name
   --     Nil_Element if Element is a Compilation_Unit
   --
   function Enclosing_Program_Unit (Element          : Asis.Element;
                                    Including_Accept : Boolean      := False)
                                   return Asis.Defining_Name;

   -- Image of a name, given either as a simple name or as a Selected_Name
   function Extended_Name_Image (Name_Elem : Asis.Element) return Wide_String;

   -- Full name of a name
   -- Works like Asis.Declarations.Defining_Name_Image,
   -- but returns the full (unique) name of The_Name, starting from the
   -- enclosing compilation unit (Standard for predefined elements).
   -- If With_Profile is true, "mangles" the name with a profile to provide a name
   -- that is unique even if overloaded.
   function Full_Name_Image (The_Name     : in Asis.Defining_Name;
                             With_Profile : in Boolean := False) return Wide_String;

   -- Image of the profile of a callable construct
   -- If name is not a callable construct, returns ""
   -- Otherwise:
   --    for a procedure, entry...:
   --       returns '{' {<Full_Name_Image (types of parameter) '}'}
   --    for a function:
   --       returns '{' {<Full_Name_Image (types of parameter) } ':' Full_Name_Image (result type) '}'
   -- With_Profile determines if the Full_Name_Image generated for parameters and result includes
   -- itself a profile.
   function Profile_Image (The_Name     : Asis.Defining_Name;
                           With_Profile : Boolean := True) return Wide_String;

   -- return the type declaraion of the ultimate ancestor type of The_Element
   -- (going up all subtype and derived type declaration).
   function Ultimate_Expression_Type (The_Element : Asis.Expression) return Asis.Declaration;

   -- Real kind of an expression
   -- return the Type_Kind of the ultimate ancestor of The_Element
   -- (going up all subtype and derived type declaration).
   function Expression_Type_Kind (The_Element : Asis.Expression) return Asis.Type_Kinds;

   -- Given a procedure, entry or function call, returns the parameter profile of the called
   -- entity.
   -- For simple cases, it is like Parameter_Profile (Corresponding_Called_Entity (Call)),
   -- but it handles cases where the call is from an access to subprogram, a dispatching operation,
   -- etc, where the called entity is not statically known (but its profile is).
   function Called_Profile (Call : Asis.Element) return Asis.Parameter_Specification_List;

   -- Given a callable entity declaration, returns a list of Defining_Name
   -- First element is the result type for a function, Nil_Element for other declarations
   -- Other elements are (in order of declaration) the *types* of the parameters.
   -- Multiple declarations are separated, i.e. "A,B : Integer" yields two entries in the table.
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

   -- Checks whether any element in the Path is a renaming
   -- Appropriate expression kinds:
   --   An_Identifier
   --   A_Selected_Component
   --   A_Function_Call
   --   An_Indexed_Component
   --   A_Slice
   function Includes_Renaming (Path : Asis.Expression) return Boolean;

   -- Checks whether the Element is a callable construct
   -- Expected elements:
   --    A_Declaration
   --    A_Definition
   --    A_Defining_Name
   function Is_Callable_Construct (Element : Asis.Element) return Boolean;

end Thick_Queries;
