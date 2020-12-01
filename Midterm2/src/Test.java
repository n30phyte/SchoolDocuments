interface ServiceA {
    void print();
}

class A implements ServiceA {

    @Override
    public void print() {
        System.out.println("Class A");
    }
}
class C extends A {

    @Override
    public void print() {
        System.out.println("Class C");
    }
}

class D implements ServiceA {
    A a;

    B b = new B();
    public D(A a) {
        this.a = a;
    }

    @Override
    public void print() {
        System.out.println("Class D");
        b.print();
        a.print();
    }
}

class B implements ServiceA {

    @Override
    public void print() {
        System.out.println("Class B");
    }
}

public class Test {

    static void test(ServiceA obj) {
        System.out.println("----------------------");
        obj.print();
    }

    public static void main(String[] args) {
        A a1 = new C();
        A a2 = new A();
        D d1 = new D(a1);
        D d2 = new D(a2);
        test(d1);
        test(d2);
    }
}


