#pragma once

#include <unordered_map>


#include "helpers.h"
#include "symbol.h"


namespace tiny {

    /** Forward declarations for the specific types.
     */
    class Type;
    class PointerType;
    class FunctionType;
    class StructType;

    struct TypeHash {
        std::uint64_t operator () (std::vector<Type *> const & x) const {
            std::hash<Type *> hasher{};
            std::uint64_t result = 0;
            for (auto i : x)
                result += hasher(i);
            return result;
        }
    }; // TypeHash

    /** Basic type class. 
     */
    class Type {
    public:

        virtual ~Type() = default;

        /** Returns the size of the type in bytes. 
         */
        virtual size_t size() const = 0;

        /** Returns whether the type is fully defined. This is by default true for all types, but we will override this behavior for structs to allow forward declarations. 
         */
        virtual bool isFullyDefined() const { return true; }

        /** Returns true if the type can convert to a boolean type. 
         */
        virtual bool convertsToBool() const { return false; }

        virtual bool isPointer() const { return false; }

        virtual bool isNumeric() const { return false; }

        virtual bool convertsImplicitlyTo(Type * other) const { return other == this; }


        static void resetTypeInformation(); 

        static Type * getType(Symbol sym) {
            auto ts = types();
            auto i = ts.find(sym);
            return i == ts.end() ? nullptr : i->second;
        }

        static Type * createAlias(Symbol sym, Type * type) {
            types()[sym] = type;
            return type;
        }

        static PointerType * getPointerTo(Type * base);

        static FunctionType * getFunction(std::vector<Type *> const & signature);

        static StructType * getOrDeclareStruct(Symbol name);

        static Type * getVoid() { return getType(Symbol::KwVoid); }

        static Type * getInt() { return getType(Symbol::KwInt); }

        static Type * getDouble() { return getType(Symbol::KwDouble); }

        static Type * getChar() { return getType(Symbol::KwChar); }

        virtual void format(std::ostream & s) const = 0;

    private:

        friend std::ostream & operator << (std::ostream & s, Type const & t) {
            t.format(s);
            return s;
        }

        static std::unordered_map<Symbol, Type *> & types() {
            static std::unordered_map<Symbol, Type *> types = initializeTypes();
            return types;
        }

        static std::unordered_map<Type *, PointerType *> & pointerTypes() {
            static std::unordered_map<Type *, PointerType *> types;
            return types;
        }

        static std::unordered_map<std::vector<Type *>, FunctionType*, TypeHash> & functionTypes() {
            static std::unordered_map<std::vector<Type *>, FunctionType*, TypeHash> types;
            return types;
        }

        static std::unordered_map<Symbol, Type *> initializeTypes();

    }; // tiny::Type


    /** Plain old data type. 
      
        Basically just a name and size of the type. We also use the class to refer to the void type, which has size 0.  

     */
    class SimpleType : public Type {
    public:

        size_t size() const override { return size_; }

        /** All PODs convert to bool (!= 0), but void does not. */
        bool convertsToBool() const override {
            return name_ != Symbol::KwVoid;
        }

        bool isNumeric() const override {
            return name_ == Symbol::KwInt || name_ == Symbol::KwDouble || name_ == Symbol::KwChar;
        }

        /** TinyC supports conversion from larger numeric type to smaller one, or from an int to pointer.
         */
        bool convertsImplicitlyTo(Type * t) const override  {
            return Type::convertsImplicitlyTo(t) ||
                (name_ == Symbol::KwInt && t->isPointer()) ||
                (name_ == Symbol::KwInt && t == Type::getDouble()) ||
                (name_ == Symbol::KwChar && t == Type::getInt()) ||
                (name_ == Symbol::KwChar && t == Type::getDouble());
        }

        void format(std::ostream & s) const override { s << name_.name(); };

    private:
        friend class Type;

        SimpleType(Symbol name, size_t size):name_{name}, size_{size} {}

        Symbol name_;
        size_t size_;

    }; // tiny::Simple

    class PointerType : public Type {
    public:

        Type * base() const {
            return base_;
        }

        size_t size() const override {
            return 4;
        }        

        /** Pointers convert to bool (not null). 
         */
        bool convertsToBool() const override { return true; }

        bool isPointer() const override { return true; }

        /** A pointer converts implicitly to int type. 
         
            Also, an implicit converstion to void * would not be very wrong in TinyC's context. 
        */
        bool convertsImplicitlyTo(Type * t) const override {
            return Type::convertsImplicitlyTo(t) ||
                t == Type::getInt();
        }

        void format(std::ostream & s) const override {  base_->format(s); s << "*"; };

    private:
        friend class Type;

        PointerType(Type * base):
            base_{base} {
        }

        Type * base_;

    }; // tiny::Pointer

    class FunctionType : public Type {
    public:

        size_t numArgs() const { return signature_.size() - 1; }

        Type * arg(size_t i) const { return signature_[i + 1]; }

        Type * returnType() const { return signature_[0]; }

        size_t size() const override { return 0; }

        void format(std::ostream & s) const override {  
            signature_[0]->format(s);
            s << "(";
            for (size_t i = 1, e = signature_.size(); i < e; ++i) {
                signature_[i]->format(s);
                if (i + 1 < e)
                    s << ", ";
            }
            s << ")";
         };

    private:

        friend class Type;

        FunctionType(std::vector<Type *> const & signature): signature_{signature} {
        }

        std::vector<Type *> signature_;

    }; // tiny::FunctionType

    class StructType : public Type {
    public:

        /** Returns true if the struct type is fully defined.
         */
        bool isFullyDefined() const override {
            return fullyDefined_;
        }

        /** Marks the struct as fully defined. */
        void markAsFullyDefined() {
            ASSERT(fullyDefined_ == false);
            fullyDefined_ = true;
            // Emptry struct still needs to occupy some memory for various purposes
            if (size_ == 0)
                size_ = 1;
        }

        /** Calculates the size of the struct and returns it. 
         */
        size_t size() const override { return size_; }

        void format(std::ostream & s) const override {
            s << "struct " << name_.name();
        }        

        /** Adds the field with given name and type to the structure. 
         
            Returns true if the field has been added correctly, false otherwise (if a field with same name already exists).
        */
        bool addField(Symbol name, Type * type) {
            ASSERT(! isFullyDefined());
            ASSERT(type->isFullyDefined());
            for (auto & i : fields_)
                if (i.first == name)
                    return false;
            fields_.push_back(std::make_pair(name, type));     
            // TODO should we do memory alignment & padding? 
            size_ += type->size();
            return true;
        }

        Type * operator[](Symbol name) const {
            for (auto & i : fields_)
                if (i.first == name)
                    return i.second;
            return nullptr;
        }
        
    private:
        friend class Type;

        StructType(Symbol name): name_{name} {}

        Symbol name_;

        bool fullyDefined_ = false;

        size_t size_ = 0;

        std::vector<std::pair<Symbol, Type *>> fields_;        

    }; // tiny::StructType


}