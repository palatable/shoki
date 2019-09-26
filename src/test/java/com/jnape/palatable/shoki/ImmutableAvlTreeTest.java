package com.jnape.palatable.shoki;

import org.junit.Test;

import java.util.Iterator;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.ImmutableAvlTree.empty;
import static com.jnape.palatable.shoki.ImmutableAvlTree.singleton;
import static com.jnape.palatable.shoki.SizeInfo.known;
import static java.util.Collections.emptyList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static testsupport.matchers.IterableMatcher.iterates;

public class ImmutableAvlTreeTest {

    @Test
    public void emptyTreeInsert() {
        assertEquals(singleton(1), ImmutableAvlTree.<Integer>empty().insert(1));
    }

    @Test
    public void singleNodeInsert() {
        ImmutableAvlTree<Integer> oneInsertZero = singleton(1).insert(0);
        assertEquals(singleton(0), oneInsertZero.left());
        assertEquals(empty(), oneInsertZero.right());

        ImmutableAvlTree<Integer> oneInsertTwo = singleton(1).insert(2);
        assertEquals(empty(), oneInsertTwo.left());
        assertEquals(singleton(2), oneInsertTwo.right());

        ImmutableAvlTree<Integer> oneInsertOne = singleton(1).insert(1);
        assertEquals(singleton(1), oneInsertOne);
    }

    @Test
    public void treeInsert() {
        ImmutableAvlTree<Integer> tree = singleton(3).insert(1).insert(2).insert(4);

        assertEquals(just(2), tree.root());
        assertEquals(singleton(1), tree.left());
        assertEquals(singleton(3).insert(4), tree.right());
    }

    @Test
    public void emptyTreeRemove() {
        assertEquals(empty(), ImmutableAvlTree.<Integer>empty().remove(1));
    }

    @Test
    public void singletonTreeRemove() {
        assertEquals(empty(), singleton(1).remove(1));
        assertEquals(singleton(1), singleton(1).remove(2));
    }

    @Test
    public void treeRemove() {
        ImmutableAvlTree<Integer> tree = singleton(3).insert(1).insert(2).insert(4);

        assertEquals(singleton(2).insert(1).insert(4), tree.remove(3));
        assertEquals(singleton(3).insert(2).insert(4), tree.remove(1));
        assertEquals(singleton(3).insert(1).insert(4), tree.remove(2));
    }

    //todo: stop unfolding when better Iterable impl for elements
//    @Test
//    public void treeRemovePCP() {
//        Iterable<Integer> elements = take(1000, iterate(x -> x + 1, 1));
//        ImmutableAvlTree<Integer> tree = foldLeft(ImmutableAvlTree::insert, empty(), elements);
//
//        elements.forEach(
//                index -> {
//                    ImmutableAvlTree<Integer> treeWithoutElement = foldLeft(ImmutableAvlTree::insert, empty(), filter(not(eq(index)), elements));
//                    Iterable<Integer> xs = treeWithoutElement.breadthFirst();
//                    Iterable<Integer> ys = tree.remove(index).breadthFirst();
//                    assertEquals("Failed on attempt #" + index, toCollection(HashSet::new, xs), toCollection(HashSet::new, ys));
//                });
//    }

    @Test
    public void sizeInfo() {
        assertEquals(known(0), empty().sizeInfo());
        assertEquals(known(1), singleton(1).sizeInfo());
        assertEquals(known(4), singleton(1).insert(0).insert(3).insert(2).sizeInfo());
    }

    @Test
    public void isEmpty() {
        assertTrue(empty().isEmpty());
        assertFalse(singleton(1).isEmpty());
        assertFalse(ImmutableAvlTree.of(1).isEmpty());
        assertFalse(ImmutableAvlTree.<Integer>empty().insert(1).isEmpty());
    }

    @Test
    public void contains() {
        assertFalse(ImmutableAvlTree.<Integer>empty().contains(1));
        assertFalse(ImmutableAvlTree.<Integer>empty().insert(2).contains(1));
        assertTrue(ImmutableAvlTree.<Integer>empty().insert(1).contains(1));
    }

    @Test
    public void height() {
        assertEquals(known((byte) 0), empty().height());
        assertEquals(known((byte) 1), singleton(1).height());
        assertEquals(known((byte) 3), singleton(1).insert(0).insert(3).insert(2).height());
    }

    @Test
    public void balancingRightHeavyWithSingleRotation() {
        ImmutableAvlTree<Integer> rightHeavy = singleton(1).insert(2);
        ImmutableAvlTree<Integer> reBalanced = rightHeavy.insert(3);

        assertEquals(just(2), reBalanced.root());
        assertEquals(singleton(1), reBalanced.left());
        assertEquals(singleton(3), reBalanced.right());
    }

    @Test
    public void balancingLeftHeavyWithSingleRotation() {
        ImmutableAvlTree<Integer> leftHeavy  = singleton(3).insert(2);
        ImmutableAvlTree<Integer> reBalanced = leftHeavy.insert(1);

        assertEquals(just(2), reBalanced.root());
        assertEquals(singleton(1), reBalanced.left());
        assertEquals(singleton(3), reBalanced.right());
    }

    @Test
    public void balancingLeftHeavyWithDoubleRotation() {
        ImmutableAvlTree<Integer> leftHeavy  = singleton(3).insert(1);
        ImmutableAvlTree<Integer> reBalanced = leftHeavy.insert(2);

        assertEquals(just(2), reBalanced.root());
        assertEquals(singleton(1), reBalanced.left());
        assertEquals(singleton(3), reBalanced.right());
    }

    @Test
    public void balancingRightHeavyWithDoubleRotation() {
        ImmutableAvlTree<Integer> rightHeavy = singleton(1).insert(3);
        ImmutableAvlTree<Integer> reBalanced = rightHeavy.insert(2);

        assertEquals(just(2), reBalanced.root());
        assertEquals(singleton(1), reBalanced.left());
        assertEquals(singleton(3), reBalanced.right());
    }

    @Test
    public void subTrees() {
        assertEquals(emptyList(), empty().branches());

        ImmutableAvlTree<Integer>           tree     = ImmutableAvlTree.of(1, 2, 3, 4, 5);
        Iterator<ImmutableAvlTree<Integer>> subTrees = tree.branches().iterator();

        assertEquals(tree.left(), subTrees.next());
        assertEquals(tree.right(), subTrees.next());
    }

    @Test
    public void emptyReusesSameInstance() {
        assertSame(empty(), empty());
    }

    @Test
    public void root() {
        assertEquals(nothing(), empty().root());
        assertEquals(just(1), ImmutableAvlTree.<Integer>empty().insert(1).root());
    }

    @Test
    public void breadthFirstLTR() {
//        assertThat(empty().breadthFirst(), IterableMatcher.isEmpty());
//        assertThat(ImmutableAvlTree.of(1, 2, 3).breadthFirst(), iterates(2, 1, 3));
        assertThat(ImmutableAvlTree.of(1, 2, 3, 4, 5, 6).breadthFirst(), iterates(4, 2, 5, 1, 3, 6));
    }
}