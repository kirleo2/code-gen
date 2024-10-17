#include "types.h"

namespace tiny {


    void Type::resetTypeInformation() {
        // clear all named types and reinitialize them
        auto & t = types();
        for (auto & i : t)
            delete i.second;
        t = initializeTypes();
        // clear all pointer types
        auto & pt = pointerTypes();
        for (auto & i : pt) 
            delete i.second;
        pt.clear();
        // clear all function types
        auto & ft = functionTypes();
        for (auto & i : ft) 
            delete i.second;
        ft.clear();
    }

    std::unordered_map<Symbol, Type *> Type::initializeTypes() {
        std::unordered_map<Symbol, Type *> result;
        result[Symbol::KwVoid] = new SimpleType{Symbol::KwVoid, 0};
        result[Symbol::KwInt] = new SimpleType{Symbol::KwInt, 8};
        result[Symbol::KwDouble] = new SimpleType{Symbol::KwDouble, 8};
        result[Symbol::KwChar] = new SimpleType{Symbol::KwChar, 1};
        return result;
    }

    PointerType * Type::getPointerTo(Type * base) {
        auto & t = pointerTypes();
        auto i = t.find(base);
        if (i == t.end()) 
            i = t.insert(std::make_pair(base, new PointerType{base})).first;
        return i->second;
    }

    FunctionType * Type::getFunction(std::vector<Type *> const & signature) {
        auto & types = functionTypes();
        auto i = types.find(signature);
        if (i == types.end())
            i = types.insert(std::make_pair(signature, new FunctionType{signature})).first;
        return i->second;
    }

    StructType * Type::getOrDeclareStruct(Symbol name) {
        auto & ts = types();
        auto i = ts.find(name);
        if (i == ts.end()) 
            i = ts.insert(std::make_pair(name, new StructType{name})).first;
        return dynamic_cast<StructType*>(i->second);
    }



} // namespace tiny