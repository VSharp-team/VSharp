using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using VSharp;
using VSharp.Core;
using listProductReg = VSharp.listProductRegion<VSharp.points<int>>;
using productReg = VSharp.productRegion<VSharp.intervals<int>, VSharp.listProductRegion<VSharp.points<int>>>;

namespace UnitTests
{
    [TestFixture]
    public sealed class RegionTreeTests
    {
        [Test]
        public void TestIntervals()
        {
            var zero = intervals<int>.Singleton<int>(0);
            var two = intervals<int>.Singleton<int>(2);
            var three = intervals<int>.Singleton<int>(3);
            var ten = intervals<int>.Singleton<int>(10);
            var seg0_10 = intervals<int>.Closed<int>(0, 10);                                                                                         // [0;10]
            var seg0_10_open = ((IRegion<intervals<int>>)((IRegion<intervals<int>>)seg0_10).Subtract(zero)).Subtract(ten);                                  // (0;10)
            var seg2_3 = intervals<int>.Closed<int>(2, 3);                                                                                           // [2;3]
            var result1 = ((IRegion<intervals<int>>) seg0_10_open).Subtract(seg2_3);                                                                        // (0;2)U(3;10)
            var seg0_2 = ((IRegion<intervals<int>>) ((IRegion<intervals<int>>) intervals<int>.Closed<int>(0, 2)).Subtract(zero)).Subtract(two);      // (0;2)
            var seg3_10 = ((IRegion<intervals<int>>) ((IRegion<intervals<int>>) intervals<int>.Closed<int>(3, 10)).Subtract(three)).Subtract(ten);   // (3;10)
            var result2 = seg3_10.Union(seg0_2);                                                                                                            // (0;2)U(3;10)
            Assert.AreEqual(result1, result2);
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<intervals<int>>)result1).CompareTo(result2));
            Assert.AreEqual(result1, ((IRegion<intervals<int>>)result1).Intersect(result2));
            var seg2_15 = intervals<int>.Closed<int>(2, 15);                                                                                         // [2;15]
            Assert.AreEqual(RegionComparisonResult.Intersects, ((IRegion<intervals<int>>)result1).CompareTo(seg2_15));
            Assert.AreEqual(seg3_10, ((IRegion<intervals<int>>)result1).Intersect(seg2_15));
            Assert.AreEqual(RegionComparisonResult.Disjoint, ((IRegion<intervals<int>>)result1).CompareTo(two));
            Assert.AreEqual(RegionComparisonResult.Disjoint, ((IRegion<intervals<int>>)result1).CompareTo(two.Union(zero)));
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<intervals<int>>)seg0_10).CompareTo(result1));
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<intervals<int>>)result1).CompareTo(result1));

            var seg2_4 = intervals<int>.Closed<int>(2, 4);
            var seg3_5 = intervals<int>.Closed<int>(3, 5);
            Assert.AreEqual(RegionComparisonResult.Intersects, ((IRegion<intervals<int>>)seg3_5).CompareTo(((IRegion<intervals<int>>)seg0_10).Subtract(seg2_4)));
        }

        [Test]
        public void TestPoints()
        {
            var zero = points<int>.Singleton(0);                    // {0}
            var one = points<int>.Singleton(1);                     // {1}
            var two = points<int>.Singleton(2);                     // {2}
            var universe = points<int>.Universe;
            var no0 = ((IRegion<points<int>>) universe).Subtract(zero);  // Z\{0}
            var no2 = ((IRegion<points<int>>) universe).Subtract(two);   // Z\{2}
            var no01 = ((IRegion<points<int>>) no0).Subtract(one);       // Z\{0,1}
            var no012 = ((IRegion<points<int>>) no01).Subtract(two);     // Z\{0,1,2}
            var empty = ((IRegion<points<int>>) no012).Subtract(no012);
            Assert.AreEqual(zero, ((IRegion<points<int>>) zero).Intersect(universe));     // {0} /\ Z = {0}
            Assert.AreEqual(empty, ((IRegion<points<int>>) universe).Subtract(universe)); // Z \ Z = empty
            Assert.AreEqual(two, ((IRegion<points<int>>) no01).Subtract(no012));          // (Z\{0,1}) \ (Z\{0,1,2}) = {2}
            Assert.AreEqual(no012, ((IRegion<points<int>>) no01).Intersect(no2));         // (Z\{0,1}) /\ (Z\{2}) = Z\{0,1,2}
            Assert.AreEqual(RegionComparisonResult.Disjoint, ((IRegion<points<int>>) no01).CompareTo(zero));
            Assert.AreEqual(RegionComparisonResult.Disjoint, ((IRegion<points<int>>) one).CompareTo(zero));
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<points<int>>) no0).CompareTo(two));
            Assert.AreEqual(RegionComparisonResult.Intersects, ((IRegion<points<int>>) two).CompareTo(no01));
            Assert.AreEqual(RegionComparisonResult.Intersects, ((IRegion<points<int>>) no0).CompareTo(no2));
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<points<int>>) zero).CompareTo(zero));
        }

        private typeWrapper WrapType(Type t)
        {
            return new typeWrapper(t);
        }

        [Test]
        public void TestTypeRegions()
        {
            var dict_int_int = typeof(Dictionary<int, int>);
            var dict_double_double = typeof(Dictionary<double, double>);
            var dict_t1_t2 = dict_int_int.GetGenericTypeDefinition();
            var t1 = dict_t1_t2.GetGenericArguments()[0];
            var t2 = dict_t1_t2.GetGenericArguments()[1];
            var dict_int_t2 = dict_t1_t2.MakeGenericType(typeof(int), t2);
            var dict_double_t2 = dict_t1_t2.MakeGenericType(typeof(double), t2);
            var dict_t1_int = dict_t1_t2.MakeGenericType(t1, typeof(int));

            var dict_int_int_reg = freeRegion<typeWrapper>.Singleton<typeWrapper>(WrapType(dict_int_int));
            var dict_double_double_reg = freeRegion<typeWrapper>.Singleton<typeWrapper>(WrapType(dict_double_double));
            var dict_t1_t2_reg = freeRegion<typeWrapper>.Singleton<typeWrapper>(WrapType(dict_t1_t2));
            var dict_int_t2_reg = freeRegion<typeWrapper>.Singleton<typeWrapper>(WrapType(dict_int_t2));
            var dict_double_t2_reg = freeRegion<typeWrapper>.Singleton<typeWrapper>(WrapType(dict_double_t2));
            var dict_t1_int_reg = freeRegion<typeWrapper>.Singleton<typeWrapper>(WrapType(dict_t1_int));

            Assert.AreEqual(dict_int_int_reg, ((IRegion<freeRegion<typeWrapper>>) dict_int_int_reg).Intersect(dict_int_int_reg));
            Assert.AreEqual(freeRegion<typeWrapper>.Empty, ((IRegion<freeRegion<typeWrapper>>) dict_int_int_reg).Subtract(dict_int_int_reg));

            Assert.AreEqual(RegionComparisonResult.Disjoint, ((IRegion<freeRegion<typeWrapper>>) dict_t1_int_reg).CompareTo(dict_double_double_reg));
            Assert.AreEqual(RegionComparisonResult.Intersects, ((IRegion<freeRegion<typeWrapper>>) dict_t1_int_reg).CompareTo(dict_t1_t2_reg));
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<freeRegion<typeWrapper>>) dict_double_t2_reg).CompareTo(dict_double_double_reg));
            Assert.AreEqual(RegionComparisonResult.Intersects, ((IRegion<freeRegion<typeWrapper>>) dict_t1_int_reg).CompareTo(dict_double_t2_reg));

            var d1 = ((IRegion<freeRegion<typeWrapper>>) dict_t1_int_reg).Intersect(dict_int_t2_reg);
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<freeRegion<typeWrapper>>) d1).CompareTo(dict_int_int_reg));
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<freeRegion<typeWrapper>>) dict_int_int_reg).CompareTo(d1));

            var d2 = ((IRegion<freeRegion<typeWrapper>>) dict_t1_int_reg).Subtract(dict_int_t2_reg);
            Assert.AreEqual(RegionComparisonResult.Disjoint, ((IRegion<freeRegion<typeWrapper>>) d2).CompareTo(dict_int_int_reg));

            var d3 = ((IRegion<freeRegion<typeWrapper>>) dict_t1_t2_reg).Subtract(dict_int_int_reg);
            var d4 = ((IRegion<freeRegion<typeWrapper>>) d3).Subtract(dict_double_double_reg);
            var d5 = ((IRegion<freeRegion<typeWrapper>>) d3).Subtract(d4);
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<freeRegion<typeWrapper>>) d5).CompareTo(dict_double_double_reg));
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<freeRegion<typeWrapper>>) dict_double_double_reg).CompareTo(d5));
        }

        [Test]
        public void TestCartesianRegions()
        {
            var zeroSeg = intervals<int>.Singleton<int>(0);
            var tenSeg = intervals<int>.Singleton<int>(10);
            var seg0_10 = intervals<int>.Closed<int>(0, 10);                                                                                         // [0;10]
            var seg0_10_open = ((IRegion<intervals<int>>)((IRegion<intervals<int>>)seg0_10).Subtract(zeroSeg)).Subtract(tenSeg);                            // (0;10)
            var seg2_3 = intervals<int>.Closed<int>(2, 3);                                                                                           // [2;3]
            var seg0_2_3_10 = ((IRegion<intervals<int>>) seg0_10_open).Subtract(seg2_3);                                                                    // (0;2)U(3;10)

            var zeroPoint = points<int>.Singleton(0);                    // {0}
            var onePoint = points<int>.Singleton(1);                     // {1}
            var universe = points<int>.Universe;
            var no0Points = ((IRegion<points<int>>) universe).Subtract(zeroPoint);  // Z\{0}
            var no1Points = ((IRegion<points<int>>) universe).Subtract(onePoint);   // Z\{1}
            var no01Points = ((IRegion<points<int>>) no0Points).Subtract(onePoint);       // Z\{0,1}

            var reg1 = productReg.ProductOf<intervals<int>, listProductReg>(seg0_10, listProductReg.OfSeq(new[] { universe, universe }));
            var reg2 = productReg.ProductOf<intervals<int>, listProductReg>(seg0_2_3_10, listProductReg.OfSeq(new[] { no1Points, no0Points }));
            var reg3 = productReg.ProductOf<intervals<int>, listProductReg>(seg0_2_3_10, listProductReg.OfSeq(new[] { no0Points, no1Points }));
            var reg4 = productReg.ProductOf<intervals<int>, listProductReg>(seg0_2_3_10, listProductReg.OfSeq(new[] { no01Points, no01Points }));
            var reg5 = productReg.ProductOf<intervals<int>, listProductReg>(seg0_2_3_10, listProductReg.OfSeq(new[] { no0Points, zeroPoint }));
            var reg6 = productReg.ProductOf<intervals<int>, listProductReg>(seg0_2_3_10, listProductReg.OfSeq(new[] { no0Points, no01Points }));
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<productReg>)reg1).CompareTo(reg3));
            Assert.AreEqual(RegionComparisonResult.Intersects, ((IRegion<productReg>)reg2).CompareTo(reg3));
            Assert.AreEqual(reg4, ((IRegion<productReg>)reg2).Intersect(reg3));
            Assert.AreEqual(reg6, ((IRegion<productReg>)reg3).Subtract(reg5));
            Assert.IsTrue(((IRegion<productReg>)((IRegion<productReg>)reg5).Subtract(reg3)).IsEmpty);

            var prodReg1 = productRegion<intervals<int>, points<int>>.ProductOf<intervals<int>, points<int>>(zeroSeg, zeroPoint);
            var prodReg2 = productRegion<intervals<int>, points<int>>.ProductOf<intervals<int>, points<int>>(zeroSeg, zeroPoint);
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<productRegion<intervals<int>, points<int>>>)prodReg1).CompareTo(prodReg2));
            Assert.AreEqual(RegionComparisonResult.Includes, ((IRegion<productRegion<intervals<int>, points<int>>>)prodReg2).CompareTo(prodReg1));
        }

        private sealed class IntKeyWrapper : IRegionTreeKey<IntKeyWrapper>
        {
            public int Key;

            public IntKeyWrapper(int key)
            {
                Key = key;
            }

            public bool Hides(IntKeyWrapper other)
            {
                return Key == other.Key;
            }
        }

        private IntKeyWrapper Wrap(int key)
        {
            return new IntKeyWrapper(key);
        }

        [Test]
        public void TestRegionTree()
        {
            var seg0_10 = intervals<int>.Closed<int>(0, 10);
            var seg2_4 = intervals<int>.Closed<int>(2, 4);
            var seg3_5 = intervals<int>.Closed<int>(3, 5);
            var seg4_5 = intervals<int>.Closed<int>(4, 5);
            var seg1_6 = intervals<int>.Closed<int>(1, 6);
            var seg0 = intervals<int>.Singleton<int>(0);
            var seg2 = intervals<int>.Singleton<int>(2);
            var seg3 = intervals<int>.Singleton<int>(3);
            var tree0 = RegionTree.empty<IntKeyWrapper, intervals<int>>();
            var tree1 = RegionTree.write(seg0_10, Wrap(0), tree0);
            var tree2 = RegionTree.write(seg2_4, Wrap(1), tree1);
            var tree3 = RegionTree.write(seg3_5, Wrap(2), tree2);
            RegionTree.checkInvariant(tree0);
            RegionTree.checkInvariant(tree1);
            RegionTree.checkInvariant(tree2);
            RegionTree.checkInvariant(tree3);
            var loc0 = RegionTree.localize(seg0, tree3);
            var loc2 = RegionTree.localize(seg2, tree3);
            var loc3 = RegionTree.localize(seg3, tree3);
            var loc4_5 = RegionTree.localize(seg4_5, tree3);
            var loc1_6 = RegionTree.localize(seg1_6, tree3);
            RegionTree.checkInvariant(regionTree<IntKeyWrapper, intervals<int>>.NewNode(loc0));
            RegionTree.checkInvariant(regionTree<IntKeyWrapper, intervals<int>>.NewNode(loc2));
            RegionTree.checkInvariant(regionTree<IntKeyWrapper, intervals<int>>.NewNode(loc3));
            RegionTree.checkInvariant(regionTree<IntKeyWrapper, intervals<int>>.NewNode(loc4_5));
            RegionTree.checkInvariant(regionTree<IntKeyWrapper, intervals<int>>.NewNode(loc1_6));
            Assert.AreEqual(1, PersistentDict.size(loc0));
            Assert.AreEqual(1, PersistentDict.size(loc2));
            Assert.AreEqual(1, PersistentDict.size(loc3));
            Assert.AreEqual(1, PersistentDict.size(loc4_5));
            Assert.AreEqual(3, PersistentDict.size(loc1_6));
            Assert.AreEqual(0, PersistentDict.toSeq(loc0).Single().Item2.Item1.Key);
            Assert.AreEqual(1, PersistentDict.toSeq(loc2).Single().Item2.Item1.Key);
            Assert.AreEqual(2, PersistentDict.toSeq(loc3).Single().Item2.Item1.Key);
            Assert.AreEqual(2, PersistentDict.toSeq(loc4_5).Single().Item2.Item1.Key);
        }
    }
}
