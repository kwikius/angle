

#ifndef __cpp_concepts 
#error requires concepts
#endif

#include <ratio>
#include <type_traits>
#include <cmath>
#include <iostream>

namespace pqs{ 
   
   // std::same_as ?
   template <typename TL,typename TR>
   concept same_as = std::is_same_v<TL,TR>;

   //see http://www.open-pqs.org/jtc1/sc22/wg21/docs/papers/2020/p0870r1.html
   template <typename From, typename To>
   inline constexpr bool is_narrowing_conversion = false;

   template<typename T>
   inline constexpr bool is_angle = false;

   template <typename T> 
   inline constexpr bool is_number = false;

   template<typename T>
   inline constexpr bool is_real_number = false;

   /**
   * to represent invalid constructs
   */
   struct undefined { undefined() = delete;};

   namespace detail{

      template<typename T>
      inline constexpr bool is_ratio = false;

      template<typename T>
      inline constexpr bool is_mathematic_angle = false;

   }

   /**
   *  concept for std::ratio
   *  not for customisation
   */
   template <typename T>
   concept in_ratio = pqs::detail::is_ratio<T>;

   /**
   *  concept for non zero ratio 
   * not for customistaion
   */
   template <in_ratio T>
   concept in_non_zero_ratio = ! same_as<T,std::ratio<0> >;

   namespace  detail{
      // implement in_ratio
      template <std::intmax_t Num, std::intmax_t Den>
      inline constexpr bool is_ratio<std::ratio<Num,Den> > = true;
   }
   /**
   * concept for generic angle
   * for customisation
   */
   template <typename T>
   concept in_angle = pqs::is_angle<T>;

   /**
   * specific mathematic_angle concept
   * not for customisation
   */
   template <typename T>
   concept in_mathematic_angle = pqs::detail::is_mathematic_angle<T>;

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
   concept in_number = pqs::is_number<T>;

   /**
   *  real number, built in floating_point + UDTs
   * can be customised
   */
   template <typename T>
   concept in_real_number = pqs::is_real_number<T>;

   /**
   *  predicate to test for narrowing conversion
   * also see http://www.open-pqs.org/jtc1/sc22/wg21/docs/papers/2020/p0870r1.html
   */
   template <in_arithmetic From,in_arithmetic To>
   inline constexpr bool is_narrowing_conversion<From,To> = ! std::is_same_v<typename std::common_type_t<From, To>, To>;

   template<pqs::in_floating_point T>
   inline constexpr bool is_real_number<T> = true;

   template <pqs::in_arithmetic T>
   inline constexpr bool is_number<T> = true;

   // forward decl : class template for plane angle solid angle
   template <
      in_real_number ValueType = double,
      in_non_zero_ratio Exponent = std::ratio<1> 
   >
   class mathematic_angle;

   template <in_real_number ValueType,in_ratio Exponent>
   inline constexpr bool is_angle<pqs::mathematic_angle<ValueType,Exponent> > = true;

   namespace detail{

      template <in_real_number ValueType,in_ratio Exponent>
      inline constexpr bool is_mathematic_angle<pqs::mathematic_angle<ValueType,Exponent> > = true;

      template <typename Lhs, typename Rhs>
      concept in_mathematic_angle_arith_pair = 
            (in_number<Lhs> && in_mathematic_angle<Rhs>) ||
            ( in_mathematic_angle<Lhs> && ( in_mathematic_angle<Rhs> || in_number<Rhs>));
   }//detail

   // exclusive namespace for mathematic_angle functions
   namespace mathematic_angle_impl{
      /**
       * @brief An exclusive base class for mathematic_angle which contains hidden Friend functions for mathematic_angle
       * without asymmetry of names,args, params caused by being in the mathematic_angle class itself
       */
      class functions{

         // prevent derivation by anything but mathematic_angle
         template <
            pqs::in_real_number ValueType,
            pqs::in_non_zero_ratio Exponent
         >
         friend class pqs::mathematic_angle;

         functions() = default;
         functions(functions const &) = default;
         functions(functions &&) = default;

         functions & operator= (functions const &) = default;
         functions & operator= (functions &&) = default;

         template <pqs::in_mathematic_angle Lhs>
         friend std::ostream & operator <<(std::ostream & os, Lhs const & lhs)
         {
            using exp = Lhs::exponent;
            return os << "mathematic_angle<" << exp::num << '/' << exp::den  << ">(" << lhs.numeric_value() << ")";
         }

         template <pqs::in_mathematic_angle Lhs>
            requires pqs::same_as<typename Lhs::exponent, std::ratio<1> >
         friend std::ostream & operator <<(std::ostream & os, Lhs const & lhs)
         {
            return os << lhs.numeric_value() << " rad";
         }

         template <pqs::in_mathematic_angle Lhs>
            requires pqs::same_as<typename Lhs::exponent, std::ratio<2> >
         friend std::ostream & operator <<(std::ostream & os, Lhs const & lhs)
         {
            return os << lhs.numeric_value() << " sr";
         }

         // addition not allowed where exponent is not same
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
         friend constexpr pqs::undefined 
         operator + (Lhs const & lhs, Rhs const & rhs) { return {};}

         template <pqs::in_mathematic_angle Lhs, pqs::in_number Rhs>
         friend constexpr pqs::undefined 
         operator + (Lhs const & lhs, Rhs const & rhs) { return {};}

         template <pqs::in_number Lhs, pqs::in_mathematic_angle Rhs>
         friend constexpr pqs::undefined 
         operator + (Lhs const & lhs, Rhs const & rhs) { return {};}
         
        // addition only where exponent is the same
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
            requires pqs::same_as<typename Lhs::exponent, typename Rhs::exponent>
         friend constexpr pqs::in_mathematic_angle 
         operator + (Lhs const & lhs, Rhs const & rhs) 
         {
            using value_type = std::common_type_t<typename Lhs::value_type,typename Rhs::value_type> ;
            using exponent = typename Lhs::exponent;
            using result_type = pqs::mathematic_angle<value_type,exponent>;
            return result_type{
               lhs.numeric_value() + rhs.numeric_value()
            };
         }

          // subtraction not allowed where exponent is not same
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
         friend constexpr pqs::undefined 
         operator - (Lhs const & lhs, Rhs const & rhs) { return {};}

         template <pqs::in_mathematic_angle Lhs, pqs::in_number Rhs>
         friend constexpr pqs::undefined 
         operator - (Lhs const & lhs, Rhs const & rhs) { return {};}

         template <pqs::in_number Lhs, pqs::in_mathematic_angle Rhs>
         friend constexpr pqs::undefined 
         operator - (Lhs const & lhs, Rhs const & rhs) { return {};}
         
         // subtraction only where exponent is the same
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
            requires pqs::same_as<typename Lhs::exponent, typename Rhs::exponent>
         friend constexpr pqs::in_mathematic_angle 
         operator - (Lhs const & lhs, Rhs const & rhs) 
         {
            using value_type = std::common_type_t<typename Lhs::value_type,typename Rhs::value_type> ;
            using exponent = typename Lhs::exponent;
            using result_type = pqs::mathematic_angle<value_type,exponent>;
            return result_type{
                lhs.numeric_value() - rhs.numeric_value()
            };
         }

         //multiplication where the result is a numeric
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
            requires  pqs::same_as<
               std::ratio_add<typename Lhs::exponent,typename Rhs::exponent>,
               std::ratio<0>
            >
         friend constexpr pqs::in_real_number
         operator * (Lhs const & lhs, Rhs const & rhs) 
         {
            return lhs.numeric_value() * rhs.numeric_value();
         }

         //multiplication where the result is a mathematic_angle to some power
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
         friend constexpr pqs::in_mathematic_angle 
         operator * (Lhs const & lhs, Rhs const & rhs) 
         {
            using value_type = std::common_type_t<typename Lhs::value_type,typename Rhs::value_type> ;
            using exponent = std::ratio_add<typename Lhs::exponent,typename Rhs::exponent>;
            using result_type = pqs::mathematic_angle<value_type,exponent>;
            return result_type {lhs.numeric_value() * rhs.numeric_value()};
         }

         // multiplication by numeric
         template <pqs::in_mathematic_angle Lhs, pqs::in_number Rhs>
         friend constexpr pqs::in_mathematic_angle
         operator * (Lhs const & lhs, Rhs const & rhs) 
         {
            using value_type = std::common_type_t<typename Lhs::value_type, Rhs> ;
            using exponent = typename Lhs::exponent;
            using result_type = pqs::mathematic_angle<value_type,exponent>;
            return result_type{lhs.numeric_value() * rhs};
         }

         // multiplication by numeric
         template <pqs::in_number Lhs, pqs::in_mathematic_angle Rhs>
         friend constexpr pqs::in_mathematic_angle 
         operator * (Lhs const & lhs, Rhs const & rhs) 
         {
            using value_type = std::common_type_t<Lhs, typename Rhs::value_type> ;
            using exponent = typename Rhs::exponent;
            using result_type = pqs::mathematic_angle<value_type,exponent>;
            return result_type{lhs * rhs.numeric_value()};
         }

         // division where the result is a numeric
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
            requires pqs::same_as<typename Lhs::exponent, typename Rhs::exponent>
         friend constexpr pqs::in_real_number 
         operator / (Lhs const & lhs, Rhs const & rhs) 
         {
            return lhs.numeric_value() / rhs.numeric_value();
         }

         // division where the result is a mathematic_angle to some power
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
         friend constexpr pqs::in_mathematic_angle
         operator / (Lhs const & lhs, Rhs const & rhs) 
         {
            using value_type = std::common_type_t<typename Lhs::value_type, typename Rhs::value_type> ;
            using exponent = std::ratio_subtract<typename Lhs::exponent,typename Rhs::exponent>;
            using result_type = pqs::mathematic_angle<value_type,exponent>;
            return result_type{lhs.numeric_value() / rhs.numeric_value()};
         }

         //division rad/ numeric
         template <pqs::in_mathematic_angle Lhs, pqs::in_number Rhs>
         friend constexpr pqs::in_mathematic_angle 
         operator / (Lhs const & lhs, Rhs const & rhs) 
         {
            using value_type = std::common_type_t<typename Lhs::value_type, Rhs> ;
            using exponent = typename Lhs::exponent;
            using result_type = pqs::mathematic_angle<value_type,exponent>;
            return result_type{lhs.numeric_value() / rhs};
         }

         // division numeric/rad
         template <pqs::in_number Lhs, pqs::in_mathematic_angle Rhs>
         friend constexpr pqs::in_mathematic_angle 
         operator / (Lhs const & lhs, Rhs const & rhs) 
         {
            using value_type = std::common_type_t<Lhs, typename Rhs::value_type> ;
            using exponent = std::ratio_subtract<std::ratio<0>,typename Rhs::exponent>;
            using result_type = pqs::mathematic_angle<value_type,exponent>;
            return result_type{lhs * rhs.numeric_value()};
         }

         //comps
   //<
         /** 
         *   here we knock out unwanted overloads
         *
         *    mathematic_angle < number  -> undefined
         *    number < mathematic_angle  -> undefined
         *    mathematic_angle < mathematic_angle  -> undefined
         */
         template <typename Lhs, typename Rhs>
            requires pqs::detail::in_mathematic_angle_arith_pair<Lhs,Rhs>
         friend constexpr pqs::undefined 
         operator < (Lhs const & lhs, Rhs const & rhs) { return {};}

         // .. and refine the one we want
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
            requires pqs::same_as<typename Lhs::exponent,typename Rhs::exponent> 
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
            requires pqs::detail::in_mathematic_angle_arith_pair<Lhs,Rhs>
         friend constexpr pqs::undefined 
         operator <= (Lhs const & lhs, Rhs const & rhs) { return {};}

         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
            requires pqs::same_as<typename Lhs::exponent, typename Rhs::exponent>
         friend constexpr bool 
         operator <= (Lhs const & lhs, Rhs const & rhs) 
         {
            return lhs.numeric_value() <= rhs.numeric_value();
         }

    //==
         /** 
          *here we knock out unwanted overloads as above
         */
         template <typename Lhs, typename Rhs>
            requires pqs::detail::in_mathematic_angle_arith_pair<Lhs,Rhs>
         friend constexpr pqs::undefined 
         operator == (Lhs const & lhs, Rhs const & rhs) { return {};}

         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
            requires pqs::same_as<typename Lhs::exponent, typename Rhs::exponent>
         friend constexpr bool 
         operator == (Lhs const & lhs, Rhs const & rhs) 
         {
            return lhs.numeric_value() == rhs.numeric_value();
         }

    //!=
         /** 
          *here we knock out unwanted overloads as above
         */
         template <typename Lhs, typename Rhs>
            requires pqs::detail::in_mathematic_angle_arith_pair<Lhs,Rhs>
         friend constexpr pqs::undefined 
         operator != (Lhs const & lhs, Rhs const & rhs) { return {};}

         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
            requires pqs::same_as<typename Lhs::exponent, typename Rhs::exponent>
         friend constexpr bool 
         operator != (Lhs const & lhs, Rhs const & rhs) 
         {
            return lhs.numeric_value() != rhs.numeric_value();
         }

     // >=
         /** 
          *here we knock out unwanted overloads as above
         */
         template <typename Lhs, typename Rhs>
            requires pqs::detail::in_mathematic_angle_arith_pair<Lhs,Rhs>
         friend constexpr pqs::undefined 
         operator >= (Lhs const & lhs, Rhs const & rhs) { return {};}

         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
            requires pqs::same_as<typename Lhs::exponent, typename Rhs::exponent>
         friend constexpr bool 
         operator >= (Lhs const & lhs, Rhs const & rhs) 
         {
            return lhs.numeric_value() >= rhs.numeric_value();
         }

    // >
         /** 
          *here we knock out unwanted overloads as above
         */
         template <typename Lhs, typename Rhs>
            requires pqs::detail::in_mathematic_angle_arith_pair<Lhs,Rhs>
         friend constexpr pqs::undefined 
         operator > (Lhs const & lhs, Rhs const & rhs) { return {};}
         template <pqs::in_mathematic_angle Lhs, pqs::in_mathematic_angle Rhs>
         friend constexpr bool 
         operator > (Lhs const & lhs, Rhs const & rhs) 
            requires pqs::same_as<typename Lhs::exponent, typename Rhs::exponent>
         {
            return lhs.numeric_value() > rhs.numeric_value();
         }

         template <intmax_t N, intmax_t D, pqs::in_mathematic_angle Q>
         // constexpr for gcc anyway
         friend  constexpr auto 
         // requires Q::value_type has pqs:: pow or pow
         pow( Q const & q)
         {
             using exponent = std::ratio_multiply<std::ratio<N,D>,typename Q::exponent>;
             using value_type = typename Q::value_type;
             using result_type = pqs::mathematic_angle<value_type, exponent>;
             using std::pow;
             return result_type{ pow(q.numeric_value(),static_cast<double>(N)/D)};
         }

         template <intmax_t N, pqs::in_mathematic_angle Q>
         // constexpr for gcc anyway
         friend constexpr auto 
         // requires Q::value_type has pqs:: pow or pow
         pow( Q const & q)
         {
            using exponent = std::ratio_multiply<std::ratio<N,1>,typename Q::exponent>;
            using value_type = typename Q::value_type;
            using result_type = pqs::mathematic_angle<value_type, exponent>;
            using std::pow;
            return result_type{ pow(q.numeric_value(),N)};
         }

         template <pqs::in_mathematic_angle Q>
            requires pqs::same_as<typename Q::exponent,std::ratio<1> >
         // constexpr for gcc anyway
         friend constexpr auto sin( Q const & q)
         {
            using std::sin;
            return sin(q.numeric_value());
         }

         template <pqs::in_mathematic_angle Q>
            requires pqs::same_as<typename Q::exponent,std::ratio<1> >
         // constexpr for gcc anyway
         friend constexpr auto cos( Q const & q)
         {
            using std::cos;
            return cos(q.numeric_value());
         }

         template <pqs::in_mathematic_angle Q>
            requires pqs::same_as<typename Q::exponent,std::ratio<1> >
         // constexpr for gcc anyway
         friend constexpr auto tan( Q const & q)
         {
            using std::tan;
            return tan(q.numeric_value());
         }
      };

   } // mathematic_angle_impl

   /**
   *  mathematic angle class template
   */
   template <in_real_number ValueType, in_non_zero_ratio Exponent>
   class mathematic_angle : public pqs::mathematic_angle_impl::functions{
   public:

      using exponent = typename Exponent::type;
      using value_type = ValueType;

      mathematic_angle() = default;
      mathematic_angle(const mathematic_angle & ) = default;
      mathematic_angle(mathematic_angle &&) = default;

      mathematic_angle& operator=(const mathematic_angle&) = default;
      mathematic_angle& operator=(mathematic_angle&&) = default;
      // implicit construction from numeric
      template <in_real_number Number>
          requires ! pqs::is_narrowing_conversion<Number,value_type>
      constexpr mathematic_angle( Number const & v)
      : m_value{v}{}
      // implicit conversion to  numeric
      constexpr operator value_type() const {return m_value;}

      constexpr mathematic_angle operator + ()const { return *this;}
      constexpr mathematic_angle operator - ()const { return - this->m_value;}
      constexpr value_type numeric_value()const { return m_value;}
    private:
      value_type m_value{};
   };

} // pqs

//struct dummy{};
int main()
{

   // implicit construction from arithmetic types
   pqs::mathematic_angle<double,std::ratio<1> > constexpr v0 = 1.2;
   static_assert(pqs::in_mathematic_angle<std::remove_const_t<decltype(v0)> > );
   std::cout << "v0 = " << v0 << '\n';
   
   pqs::mathematic_angle<double,std::ratio<2> > constexpr v1 = 1.2;
   static_assert(pqs::in_mathematic_angle<std::remove_const_t<decltype(v1)> > );
   std::cout << "v1 = " << v1 << '\n';

   pqs::in_mathematic_angle constexpr v1a = v1 + v1;
   static_assert(pqs::in_mathematic_angle<std::remove_const_t<decltype(v1a)> > );
   std::cout << "v1a = " << v1a << '\n';

   auto constexpr v1b = v1 * v1;
   static_assert(pqs::in_mathematic_angle<std::remove_const_t<decltype(v1b)> > );
   std::cout << "v1b = " << v1b << '\n';

// n/a
//   auto constexpr v1c = v1 + v1b ;


   auto constexpr v2 = v1 * 2;
   static_assert(pqs::in_mathematic_angle<std::remove_const_t<decltype(v2)> > );
   std::cout << "v2 = " << v2 << '\n';

   auto constexpr v3 = 2 * v1;
   auto constexpr v4 = v2 * v3;
  // n/a
 //  auto constexpr v4a = v2 + v4;
   auto constexpr v5 = 1.0 / v1;
   auto constexpr v6 = v1 / 2.0;
   auto constexpr v7 = v1 / v1;
   static_assert(pqs::in_number<std::remove_const_t<decltype(v7)> >,"");
   auto constexpr v8 = v1 * v5;

  // n/a
  // auto constexpr v9 = v1 + 1.0;
  // auto constexpr v9a = 1.0 + v1;

  // auto constexpr v9b = v1 - 1.0;
  // auto constexpr v9c = 1.0 - v1;

  //compare
   auto constexpr b1 = v1 < v2;
   auto constexpr b2 = v1 <= v2;
   auto constexpr b3 = v1 == v2;
   auto constexpr b4 = v1 != v2;
   auto constexpr b5 = v1 >= v2;
   auto constexpr b6 = v1 > v2;

  // n/a
  // auto constexpr b6a = v4 < v2;
  // auto constexpr b7 = v1 < 1.0;

   double constexpr x = v1;

   pqs::in_angle a = v1;

   pqs::mathematic_angle<double,std::ratio<2> > aa = pow<1,2>(v4);
  
   a = pow<1,2>(v4);

   aa = pow<2,1>(pow<1,2>(a));
  
   pqs::in_angle b = v1 + v1;

   pqs::mathematic_angle<double,std::ratio<-2> > constexpr v10 = 1.2;
   pqs::in_angle v11 = v1 + v2;

   // deal with narrowing?
   pqs::mathematic_angle<float,std::ratio<2> > constexpr v12 = 1.2f;


   // should fail
  // pqs::mathematic_angle<int,std::ratio<2> > constexpr ri = 1;

}

