
#include <ratio>
#include <type_traits>
#include <cmath>
#include <iostream>

/*
   angle is a mathemetical quantity, rather than a physical quantity
   https://en.wikipedia.org/wiki/Mathematical_object
   object in a mathematical space
   Physical quantity requires a measurement of physical phenomena
   else the numeric value will not have meaning
   Angle can be measured without knowing any unit
*/

namespace std{ namespace experimental {
   
   template <typename TL,typename TR>
   concept same_as = std::is_same_v<TL,TR>;

   template <typename From, typename To>
   inline constexpr bool is_narrowing_conversion = false;

   struct undefined { undefined() = delete;};

   namespace detail{

      template<typename T>
      inline constexpr bool is_ratio = false;

      template<typename T>
      inline constexpr bool is_angle = false;

      template<typename T>
      inline constexpr bool is_radian = false;

      template <typename T> 
      inline constexpr bool is_number = false;

      template<typename T>
      inline constexpr bool is_real_number = false;
   }

   /**
   *  concept for std::ratio
   *  not for customisation
   */

   template <typename T>
   concept in_ratio = std::experimental::detail::is_ratio<T>;

   /**
   *  only non zero ratio allowed
   */
   template <in_ratio T>
   concept in_non_zero_ratio = ! same_as<T,std::ratio<0> >;

   namespace  detail{
      // implement in_ratio
      template <std::intmax_t Num, std::intmax_t Den>
      inline constexpr bool is_ratio<std::ratio<Num,Den> > = true;

   }
   /**
   *   generic angle concept
   */
   template <typename T>
   concept in_angle = std::experimental::detail::is_angle<T>;

   /**
   *  specific radian concept maybe rename to math_angle
   * not for customisation
   */
   template <typename T>
   concept in_mathematic_angle = std::experimental::detail::is_radian<T>;

   /**
   * builtin floating point are models of floating point
   * not for customisation
   */
   template <typename T>
   concept in_floating_point = std::is_floating_point_v<T>;

   /**
   *  builtin arithmetic are model of arithmetic
   * not for customisation
   */
   template <typename T>
   concept in_arithmetic = std::is_arithmetic_v<T>;

   /**
   *  number - builtin arithmetic + UDT's
   *  can be customised
   */
   template <typename T>
   concept in_number = std::experimental::detail::is_number<T>;

   /**
   *  real number, built in floating_point + UDTs
   * can be customised
   */
   template <typename T>
   concept in_real_number = std::experimental::detail::is_real_number<T>;

   /**
   *  predicate to test for narrowing conversion
   * also see http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2020/p0870r1.html
   */
   template <in_arithmetic From,in_arithmetic To>
   inline constexpr bool is_narrowing_conversion<From,To> = ! std::is_same_v<typename std::common_type_t<From, To>, To>;

   namespace detail{

       template<std::experimental::in_floating_point T>
       inline constexpr bool is_real_number<T> = true;

       template <std::experimental::in_arithmetic T>
       inline constexpr bool is_number<T> = true;

   }

   // forward decl : class template for plane angle solid angle
   template <
      in_real_number ValueType = double,
      in_non_zero_ratio Exponent = std::ratio<1> 
   >
   class radian;

   namespace detail{

      template <in_real_number ValueType,in_ratio Exponent>
      inline constexpr bool is_radian<std::experimental::radian<ValueType,Exponent> > = true;

      template <in_real_number ValueType,in_ratio Exponent>
      inline constexpr bool is_angle<std::experimental::radian<ValueType,Exponent> > = true;

      template <typename Lhs, typename Rhs>
      concept in_radian_arith_pair = (in_number<Lhs> && in_mathematic_angle<Rhs>)
                        || ( in_mathematic_angle<Lhs> && ( in_mathematic_angle<Rhs> || in_number<Rhs>));
   }

   /**
    * @brief An exclusive base class for radian which can contain hidden Friend functions for radian
    * without asymmetry of names,args, params caused by being in the radian class itself
    * TODO : move to another namespace for less cluttered lookup. Does that make a difference?
    */
   class radian_base{

      // prevent derivation by anything but radian
      template <
         in_real_number ValueType,
         in_non_zero_ratio Exponent
      >
      friend class radian;

      radian_base() = default;
      radian_base(radian_base const &) = default;
      radian_base(radian_base &&) = default;

      radian_base & operator= (radian_base const &) = default;
      radian_base & operator= (radian_base &&) = default;

      template <in_mathematic_angle Lhs>
      friend std::ostream & operator <<(std::ostream & os, Lhs const & lhs)
      {
         using exp = Lhs::exponent;
         return os << "radian<" << exp::num << '/' << exp::den  << ">(" << lhs.numeric_value() << ")";
      }

      template <in_mathematic_angle Lhs>
         requires same_as<typename Lhs::exponent, std::ratio<1> >
      friend std::ostream & operator <<(std::ostream & os, Lhs const & lhs)
      {
         return os << lhs.numeric_value() << " rad";
      }

      template <in_mathematic_angle Lhs>
         requires same_as<typename Lhs::exponent, std::ratio<2> >
      friend std::ostream & operator <<(std::ostream & os, Lhs const & lhs)
      {
         return os << lhs.numeric_value() << " sr";
      }

      // addition not allowed where exponent is not same
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
      friend constexpr undefined 
      operator + (Lhs const & lhs, Rhs const & rhs) { return {};}
      
     // addition only where exponent is the same
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
         requires same_as<typename Lhs::exponent, typename Rhs::exponent>
      friend constexpr in_mathematic_angle 
      operator + (Lhs const & lhs, Rhs const & rhs) 
      {
         using value_type = std::common_type_t<typename Lhs::value_type,typename Rhs::value_type> ;
         using exponent = typename Lhs::exponent;
         using result_type = std::experimental::radian<value_type,exponent>;
         return result_type{
            lhs.numeric_value() + rhs.numeric_value()
         };
      }

       // subtraction not allowed where exponent is not same
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
      friend constexpr undefined 
      operator - (Lhs const & lhs, Rhs const & rhs) { return {};}
      
      // subtraction only where exponent is the same
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
         requires same_as<typename Lhs::exponent, typename Rhs::exponent>
      friend constexpr in_mathematic_angle 
      operator - (Lhs const & lhs, Rhs const & rhs) 
      {
         using value_type = std::common_type_t<typename Lhs::value_type,typename Rhs::value_type> ;
         using exponent = typename Lhs::exponent;
         using result_type = std::experimental::radian<value_type,exponent>;
         return result_type{
             lhs.numeric_value() - rhs.numeric_value()
         };
      }

      //multiplication where the result is a numeric
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
         requires  same_as<
            std::ratio_add<typename Lhs::exponent,typename Rhs::exponent>,
            std::ratio<0>
         >
      friend constexpr in_real_number
      operator * (Lhs const & lhs, Rhs const & rhs) 
      {
         return lhs.numeric_value() * rhs.numeric_value();
      }

      //multiplication where the result is a radian to some power
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
      inline constexpr in_mathematic_angle 
      friend operator * (Lhs const & lhs, Rhs const & rhs) 
      {
         using value_type = std::common_type_t<typename Lhs::value_type,typename Rhs::value_type> ;
         using exponent = std::ratio_add<typename Lhs::exponent,typename Rhs::exponent>;
         using result_type = std::experimental::radian<value_type,exponent>;
         return result_type {lhs.numeric_value() * rhs.numeric_value()};
      }

      // multiplication by numeric
      template <in_mathematic_angle Lhs, in_number Rhs>
      constexpr in_mathematic_angle
      friend operator * (Lhs const & lhs, Rhs const & rhs) 
      {
         using value_type = std::common_type_t<typename Lhs::value_type, Rhs> ;
         using exponent = typename Lhs::exponent;
         using result_type = std::experimental::radian<value_type,exponent>;
         return result_type{lhs.numeric_value() * rhs};
      }

      // multiplication by numeric
      template <in_number Lhs, in_mathematic_angle Rhs>
      constexpr in_mathematic_angle 
      friend operator * (Lhs const & lhs, Rhs const & rhs) 
      {
         using value_type = std::common_type_t<Lhs, typename Rhs::value_type> ;
         using exponent = typename Rhs::exponent;
         using result_type = std::experimental::radian<value_type,exponent>;
         return result_type{lhs * rhs.numeric_value()};
      }

      // division where the result is a numeric
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
         requires same_as<typename Lhs::exponent, typename Rhs::exponent>
      constexpr in_real_number 
      friend operator / (Lhs const & lhs, Rhs const & rhs) 
      {
         return lhs.numeric_value() / rhs.numeric_value();
      }

      // division where the result is a radian to some power
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
      constexpr in_mathematic_angle
      friend operator / (Lhs const & lhs, Rhs const & rhs) 
      {
         using value_type = std::common_type_t<typename Lhs::value_type, typename Rhs::value_type> ;
         using exponent = std::ratio_subtract<typename Lhs::exponent,typename Rhs::exponent>;
         using result_type = std::experimental::radian<value_type,exponent>;
         return result_type{lhs.numeric_value() / rhs.numeric_value()};
      }

      //division rad/ numeric
      template <in_mathematic_angle Lhs, in_number Rhs>
      constexpr in_mathematic_angle 
      friend operator / (Lhs const & lhs, Rhs const & rhs) 
      {
         using value_type = std::common_type_t<typename Lhs::value_type, Rhs> ;
         using exponent = typename Lhs::exponent;
         using result_type = std::experimental::radian<value_type,exponent>;
         return result_type{lhs.numeric_value() / rhs};
      }

      // division numeric/rad
      template <in_number Lhs, in_mathematic_angle Rhs>
      constexpr in_mathematic_angle 
      friend operator / (Lhs const & lhs, Rhs const & rhs) 
      {
         using value_type = std::common_type_t<Lhs, typename Rhs::value_type> ;
         using exponent = std::ratio_subtract<std::ratio<0>,typename Rhs::exponent>;
         using result_type = std::experimental::radian<value_type,exponent>;
         return result_type{lhs * rhs.numeric_value()};
      }

      //comps
//<
      /** 
      *   here we knock out unwanted overloads
      *
      *    radian < number  -> undefined
      *    number < radian  -> undefined
      *    radian < radian  -> undefined
      */
      template <typename Lhs, typename Rhs>
         requires detail::in_radian_arith_pair<Lhs,Rhs>
      constexpr undefined 
      friend operator < (Lhs const & lhs, Rhs const & rhs) { return {};}

      // .. and refine the one we want
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
         requires same_as<typename Lhs::exponent,typename Rhs::exponent> 
      constexpr bool 
      friend operator < (Lhs const & lhs, Rhs const & rhs) 
      {
         return lhs.numeric_value() < rhs.numeric_value();
      }

//<=
      /** 
       *here we knock out unwanted overloads as above
      */
      template <typename Lhs, typename Rhs>
         requires detail::in_radian_arith_pair<Lhs,Rhs>
      constexpr undefined 
      friend operator <= (Lhs const & lhs, Rhs const & rhs) { return {};}

      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
         requires same_as<typename Lhs::exponent, typename Rhs::exponent>
      constexpr bool 
      friend operator <= (Lhs const & lhs, Rhs const & rhs) 
      {
         return lhs.numeric_value() <= rhs.numeric_value();
      }

 //==
      /** 
       *here we knock out unwanted overloads as above
      */
      template <typename Lhs, typename Rhs>
         requires detail::in_radian_arith_pair<Lhs,Rhs>
      constexpr undefined 
      friend operator == (Lhs const & lhs, Rhs const & rhs) { return {};}

      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
         requires same_as<typename Lhs::exponent, typename Rhs::exponent>
      constexpr bool 
      friend operator == (Lhs const & lhs, Rhs const & rhs) 
      {
         return lhs.numeric_value() == rhs.numeric_value();
      }

 //!=
      /** 
       *here we knock out unwanted overloads as above
      */
      template <typename Lhs, typename Rhs>
         requires detail::in_radian_arith_pair<Lhs,Rhs>
      constexpr undefined 
      friend operator != (Lhs const & lhs, Rhs const & rhs) { return {};}

      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
         requires same_as<typename Lhs::exponent, typename Rhs::exponent>
      constexpr bool 
      friend operator != (Lhs const & lhs, Rhs const & rhs) 
      {
         return lhs.numeric_value() != rhs.numeric_value();
      }

  // >=
      /** 
       *here we knock out unwanted overloads as above
      */
      template <typename Lhs, typename Rhs>
         requires detail::in_radian_arith_pair<Lhs,Rhs>
      constexpr undefined 
      friend operator >= (Lhs const & lhs, Rhs const & rhs) { return {};}

      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>
         requires same_as<typename Lhs::exponent, typename Rhs::exponent>
      constexpr bool 
      friend operator >= (Lhs const & lhs, Rhs const & rhs) 
      {
         return lhs.numeric_value() >= rhs.numeric_value();
      }

 // >
      /** 
       *here we knock out unwanted overloads as above
      */
      template <typename Lhs, typename Rhs>
         requires detail::in_radian_arith_pair<Lhs,Rhs>
      constexpr undefined 
      friend operator > (Lhs const & lhs, Rhs const & rhs) { return {};}
      template <in_mathematic_angle Lhs, in_mathematic_angle Rhs>

      constexpr bool 
      friend operator > (Lhs const & lhs, Rhs const & rhs) 
         requires same_as<typename Lhs::exponent, typename Rhs::exponent>
      {
         return lhs.numeric_value() > rhs.numeric_value();
      }

      template <intmax_t N, intmax_t D, in_mathematic_angle Q>
      // constexpr for gcc anyway
      constexpr auto 
      // requires Q::value_type has std:: pow or pow
      friend pow( Q const & q)
      {
          using exponent = std::ratio_multiply<std::ratio<N,D>,typename Q::exponent>;
          using value_type = typename Q::value_type;
          using result_type = radian<value_type, exponent>;
          using std::pow;
          return result_type{ pow(q.numeric_value(),static_cast<double>(N)/D)};
      }

      template <intmax_t N, in_mathematic_angle Q>
      // constexpr for gcc anyway
      constexpr auto 
      // requires Q::value_type has std:: pow or pow
      friend pow( Q const & q)
      {
          using exponent = std::ratio_multiply<std::ratio<N,1>,typename Q::exponent>;
          using value_type = typename Q::value_type;
          using result_type = radian<value_type, exponent>;
          using std::pow;
          return result_type{ pow(q.numeric_value(),N)};
      }

      template <in_mathematic_angle Q>
         requires same_as<typename Q::exponent,std::ratio<1> >
      // constexpr for gcc anyway
      constexpr auto friend sin( Q const & q)
      {
         using std::sin;
         return sin(q.numeric_value());
      }

      template <in_mathematic_angle Q>
         requires same_as<typename Q::exponent,std::ratio<1> >
      // constexpr for gcc anyway
      constexpr auto friend cos( Q const & q)
      {
         using std::cos;
         return cos(q.numeric_value());
      }

      template <in_mathematic_angle Q>
          requires same_as<typename Q::exponent,std::ratio<1> >
      // constexpr for gcc anyway
      constexpr auto friend tan( Q const & q)
      {
         using std::tan;
         return tan(q.numeric_value());
      }

   };

   template <in_real_number ValueType, in_non_zero_ratio Exponent>
   class radian : public radian_base{
   public:
      using exponent = typename Exponent::type;
      using value_type = ValueType;

      radian() = default;
      radian(const radian & ) = default;
      radian(radian &&) = default;

      radian& operator=(const radian&) = default;
      radian& operator=(radian&&) = default;
      // implicit construction from numeric
      template <in_real_number Number>
          requires ! std::experimental::is_narrowing_conversion<Number,value_type>
      constexpr radian( Number const & v)
      : m_value{v}{}
      // implicit conversion to  numeric
      constexpr operator value_type() const {return m_value;}

      constexpr radian operator + ()const { return *this;}
      constexpr radian operator - ()const { return - this->m_value;}
      constexpr value_type numeric_value()const { return m_value;}

      value_type m_value{};
   };

}} // std::experimental


//struct dummy{};
int main()
{
   // implicit construction from arithmetic types
   std::experimental::radian<double,std::ratio<1> > constexpr v0 = 1.2;
   static_assert(std::experimental::in_mathematic_angle<std::remove_const_t<decltype(v0)> > );
   std::cout << "v0 = " << v0 << '\n';
   
   std::experimental::radian<double,std::ratio<2> > constexpr v1 = 1.2;
   static_assert(std::experimental::in_mathematic_angle<std::remove_const_t<decltype(v1)> > );
   std::cout << "v1 = " << v1 << '\n';

   std::experimental::in_mathematic_angle constexpr v1a = v1 + v1;
   static_assert(std::experimental::in_mathematic_angle<std::remove_const_t<decltype(v1a)> > );
   std::cout << "v1a = " << v1a << '\n';

   auto constexpr v1b = v1 * v1;
   static_assert(std::experimental::in_mathematic_angle<std::remove_const_t<decltype(v1b)> > );
   std::cout << "v1b = " << v1b << '\n';

//   auto constexpr v1c = v1 + v1b ;
//   static_assert(std::experimental::in_arithmetic<std::remove_const_t<decltype(v1c)> > );
//   std::cout << "v1c = " << v1c << '\n';

   auto constexpr v2 = v1 * 2;
   static_assert(std::experimental::in_mathematic_angle<std::remove_const_t<decltype(v2)> > );
   std::cout << "v2 = " << v2 << '\n';

   auto constexpr v3 = 2 * v1;
   auto constexpr v4 = v2 * v3;
 //  auto constexpr v4a = v2 + v4;
   auto constexpr v5 = 1.0 / v1;
   auto constexpr v6 = v1 / 2.0;
   auto constexpr v7 = v1 / v1;
   auto constexpr v8 = v1 * v5;

   // implicit add/subtract with real
   auto constexpr v9 = v1 + 1.0;
   auto constexpr v9a = 1.0 + v1;

   auto constexpr v9b = v1 - 1.0;
   auto constexpr v9c = 1.0 - v1;

  //compare
   auto constexpr b1 = v1 < v2;
   auto constexpr b2 = v1 <= v2;
   auto constexpr b3 = v1 == v2;
   auto constexpr b4 = v1 != v2;
   auto constexpr b5 = v1 >= v2;
   auto constexpr b6 = v1 > v2;

 //  auto constexpr b6a = v4 < v2;

   // N.B implicit conversion from real
   // implicit conversion to real
  // auto constexpr b7 = v1 < 1.0;

   double constexpr x = v1;

   std::experimental::in_angle a = v1;

   std::experimental::radian<double,std::ratio<2> > aa = pow<1,2>(v4);
  
   a = pow<1,2>(v4);

   aa = pow<2,1>(pow<1,2>(a));
  
   std::experimental::in_angle b = v1 + v1;

   std::experimental::radian<double,std::ratio<-2> > constexpr v10 = 1.2;
   std::experimental::in_angle v11 = v1 + v2;

   // deal with narrowing?
   std::experimental::radian<float,std::ratio<2> > constexpr v12 = 1.2f;


   // should fail
   //std::experimental::radian<int,std::ratio<2> > constexpr ri = 1;

}

