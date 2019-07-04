----------------------------------------------------------------------
--  Elements_Set - Package specification                            --
--  Copyright (C) 2012, 2019 Adalog                                 --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                          Tel: +33 1 45 29 21 52          --
--  2 rue du Docteur Lombard        Fax: +33 1 45 29 25 00          --
--  92441 ISSY LES MOULINEAUX CEDEX E-m: info@adalog.fr             --
--  FRANCE                          URL: http://www.adalog.fr       --
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

with   -- Ada
  Ada.Strings.Wide_Unbounded;

with   -- Asis
  Asis;

with   -- Adalog components
  Thick_Queries;

private with
  Binary_Map;
package Elements_Set is
   -- This package implements sets of (named) Asis elements.

   type Set is private;   -- Object semantic, initially empty
   Empty_Set : constant Set;

   type Image_List is array (Asis.List_Index range <>) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   procedure Add (To : in out Set; Element  : Asis.Element);
   procedure Add (To : in out Set; Elements : Asis.Element_List);
   -- Adds Element to the set
   -- Expected element kinds:
   -- Whatever is appropriate for Thick_Queries.Full_Name_Image

   function Size (S : Set) return Asis.ASIS_Natural;

   function Names_In_Set    (S : Set) return Image_List;
   function Elements_In_Set (S : Set) return Thick_Queries.General_Defining_Name_List;
   function Elements_In_Set (S : Set) return Asis.Defining_Name_List;
   -- For the case where we know damn well that there are no attributes (not type names f.e.)

   function Contains (S : in Set; Element : in Asis.Element) return Boolean;

   procedure Delete (To : in out Set; Element : Asis.Element);

   procedure Clear (The_Set : in out Set);
private
   use Ada.Strings.Wide_Unbounded;
   package Element_Map is new Binary_Map (Key_Type   => Unbounded_Wide_String,
                                          Value_Type => Thick_Queries.General_Defining_Name);
   type Set is new Element_Map.Map;
   Empty_Set : constant Set := Set (Element_Map.Empty_Map);
end Elements_Set;
